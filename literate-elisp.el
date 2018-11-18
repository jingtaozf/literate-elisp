;;; -*- lexical-binding: t -*-
;;; literate-reader.el --- A customizable Lisp reader for Emacs org file.

;; Modified by (C) 2018 Jingtao Xu
;; Copyright (C) 2016 Mihai Bazon

;; Author: Mihai Bazon <mihai.bazon@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This replaces Elisp's `read' function, which is implemented in C,
;; with one implemented in Elisp which supports customizable syntax.
;; The way it does this is quite unorthodox -- it implements a rather
;; complete Elisp reader, but we do fall back to the original reader
;; for certain cases (like literal strings, characters, and other
;; syntax which the original reader supports but appears to be
;; internal to Emacs itself, such as byte-compiled code).
;;
;; It works nicely, if a bit slow.  To make it much faster you should
;; byte-compile this file:
;;
;;    emacs --batch --eval '(byte-compile-file "literate-reader.el")'
;;
;;; Code:

(defvar *literate-orig-read* (symbol-function #'read)
  "Remember the original `read' function, because we'll have to
use it in some situations that can't be handled from Lisp code.")

(defvar *literate-read-filename* nil
  "This dynamic variable will be bound by our read functions
while parsing is in progress.  It'll contain the value of
`load-file-name', or the name of the current buffer if it doesn't
have an associated file.")

(defvar *literate-read-inside-org-code-blocks* nil
  "This dynamic variable will be bound by our read functions
while parsing is in progress.  It'll indicate whether we are inside a elisp code blocks or not.")

(defvar literate-reader-debug-p t)

(defun literate-make-stream (in)
  "Given an input stream (which can be a buffer, a marker, a
string, a function, or t or nil--see Elisp docs) this returns the
stream as a function of one optional argument.  When called with
no arguments, this function should return the next character from
the stream.  When called with a non-nil argument (character),
this function should arrange that character to be returned on
next invokation with no arguments.

The Elisp docs aren't entirely clear about this, but the closures
returned by this function will be able to push back multiple
characters.  Also, when the input argument is a string, the
produced function will support a keyword :pos argument, which
when passed it will return the current (zero-based) position of
the stream.  Example:

  (let ((stream (literate-make-stream \"foo\")))
    (message \"%c%c\" (funcall stream) (funcall stream))  ;; fo
    (message \"%d\" (funcall stream :pos)))               ;; 2

This helps us implement `read-from-string', which has to return
the position of the stream."
  (let ((unget nil))
    (when (symbolp in)
      (setq in (symbol-function in)))
    (cond
      ((bufferp in) (lambda (&optional ch)
                      (with-current-buffer in
                        (cond
                          (ch (push ch unget))
                          (unget (pop unget))
                          (t
                           (when (not (eobp))
                             (prog1 (char-after)
                               (forward-char 1))))))))
      ((markerp in) (lambda (&optional ch)
                      (with-current-buffer (marker-buffer in)
                        (cond
                          (ch (push ch unget))
                          (unget (pop unget))
                          (t
                           (when (< (marker-position in) (point-max))
                             (prog1 (char-after in)
                               (move-marker in
                                            (1+ (marker-position in))
                                            (marker-buffer in)))))))))
      ((stringp in) (let ((pos 0))
                      (lambda (&optional ch)
                        (cond
                          ((eq ch :pos)
                           (if (< pos (length in))
                               (- pos 1)
                             pos))
                          (ch (push ch unget))
                          (unget (pop unget))
                          ((< pos (length in))
                           (prog1 (aref in pos)
                             (setq pos (1+ pos))))))))
      ((functionp in) (lambda (&optional ch)
                        (cond
                          (ch (push ch unget))
                          (unget (pop unget))
                          (t (funcall in)))))
      (t
       (read-string "Lisp expression:")))))

(defun literate-orig-read (in)
  "Calls the original (low-level C) `read'.  This function should
be invoked only within the dynamic extent of some `read' or
`read-from-string' execution."
  (funcall *literate-orig-read* in))

(defun literate-peek (in)
  "Given a stream function, return the next character without
dropping it from the stream."
  (let ((ch (funcall in)))
    (funcall in ch)
    ch))

(defun literate-next (in)
  "Given a stream function, return and discard the next
character."
  (funcall in))

(defun literate-read-while (in pred)
  "Read and return a string from the input stream, as long as the
predicate--which will be called for each character--returns
true."
  (let ((chars (list)) ch)
    (while (and (setq ch (literate-peek in))
                (funcall pred ch))
      (push (literate-next in) chars))
    (apply #'string (nreverse chars))))

(defun literate-croak (msg &rest args)
  "Error out in case of parse error."
  (apply #'error msg args))

(defun literate-read-string (in)
  "Read a string from the current stream.  It defers to
`literate-orig-read' and thus this should only be called within the
dynamic extent of some `read' function."
  (literate-orig-read in))

(defun literate-read-char ()
  "Read a character from the current stream.  It defers to
`literate-orig-read' and thus this should only be called within the
dynamic extent of some `read' function."
  (literate-orig-read in))

(defun literate-letter? (ch)
  "Tests whether the given character is a Unicode letter."
  (memq (get-char-code-property ch 'general-category)
        '(Ll Lu Lo Lt Lm Mn Mc Me Nl)))

(defun literate-whitespace? (ch)
  "Tests if the given character is whitespace (XXX actually not
all Unicode whitespace chars are handled; I'm not even sure that
would be correct)."
  (memq ch '(?  ?\t ?\n ?\f ?\r #xa0)))

(defun literate-digit? (ch)
  "Tests if the given character is a plain digit."
  (<= ?0 ch ?9))

(defun literate-number? (str)
  "Tests if the given string should be interpreted as number."
  (string-match "^[-+]?\\(?:\\(?:[0-9]+\\|[0-9]*\\.[0-9]+\\)\\(?:[E|e][+|-]?[0-9]+\\)?\\)$" str))

(defun literate-skip-whitespace (in)
  "Skip whitespace in the given stream."
  (literate-read-while in #'literate-whitespace?))

(defun literate-read-symbol-name (in)
  "Read and return the name of a symbol."
  (literate-read-while in (lambda (ch)
                      (cond
                        ((eq ch ?\\)
                         (literate-next in)
                         (if (literate-peek in) t (literate-croak "Unterminated input")))
                        (t
                         (or (literate-letter? ch)
                             (literate-digit? ch)
                             (memq ch '(?- ?+ ?= ?* ?/ ?_ ?~ ?! ?@ ?. ?\|
                                        ?$ ?% ?^ ?& ?: ?< ?> ?{ ?} ?\?))))))))

(defun literate-read-integer (in)
  "Read and return an integer (NIL if there is no integer at
current position in stream)."
  (let ((num (literate-read-while in #'literate-digit?)))
    (when (< 0 (length num))
      (string-to-number num))))

(defun literate-skip-comment (in)
  "Skip over a comment (move to end-of-line)."
  (literate-read-while in (lambda (ch)
                      (not (eq ch ?\n)))))

(defun literate-read-symbol (in)
  "Reads a symbol or a number.  If what follows in the stream
looks like a number, a number will be returned (via the original
reader)."
  (let ((name (literate-read-symbol-name in)))
    (cond
      ((literate-number? name)
       (funcall *literate-orig-read* name))
      ((zerop (length name))
       '##)
      (t
       (intern name)))))

(defun literate-read-list (in end &optional no-dot)
  "Read a list of elements from the input stream, until the end
character has been observed.  If `no-dot' is nil then it will
support a dot character before the last element, producing an
\"improper\" list.  If `no-dot' is true, then if a single dot
character is encountered this will produce an error."
  (let ((ret nil) (p nil) ch)
    (catch 'exit
      (while t
        (literate-skip-whitespace in)
        (setq ch (literate-peek in))
        (cond
          ((not ch)
           (literate-croak "Unterminated list"))
          ((eq ch end)
           (literate-next in)
           (throw 'exit ret))
          ((eq ch ?\;)
           (literate-skip-comment in))
          (t
           (let ((x (literate-read-datum in)))
             (cond
               ((eq x '\.)
                (cond
                  (no-dot (literate-croak "Dot in wrong context"))
                  (t
                   (rplacd p (literate-read-datum in))
                   (literate-skip-whitespace in)
                   (setq ch (literate-next in))
                   (unless (eq ch end)
                     (literate-croak "Dot in wrong context"))
                   (throw 'exit ret))))
               (t
                (let ((cell (cons x nil)))
                  (setq p (if ret
                              (rplacd p cell)
                            (setq ret cell)))))))))))))

(defvar org-elisp-begin-src-id "#+BEGIN_SRC elisp")
(defvar org-elisp-end-src-id "#+END_SRC")
(defun literate-read-org-options (options)
  (let ((in (literate-make-stream options)))
    (loop for token = (progn (literate-skip-whitespace in)
                             (literate-read-symbol in))
          until (eq '## token)
          collect token)))
(defun literate-tangel-p (flag)
  (case flag
    (no nil)
    (t t)))

(defun literate-read-datum (in)
  "Read and return a Lisp datum from the input stream."
  (literate-skip-whitespace in)
  (let ((ch (literate-peek in)))
    (cond
      ((not ch)
       (literate-croak "End of file during parsing"))
      ((and (not *literate-read-inside-org-code-blocks*)
            (not (eq ch ?\#)))
       (let ((line (literate-skip-comment in)))
         (when literate-reader-debug-p
           (message "ignore line %s" line))))
      ((eq ch ?\#)
       (literate-next in)
       (cond ((not *literate-read-inside-org-code-blocks*)
              (if (loop for i from 1 below (length org-elisp-begin-src-id)
                        for c1 = (aref org-elisp-begin-src-id i)
                        for c2 = (literate-next in)
                        thereis (not (char-equal c1 c2)))
                (literate-skip-comment in)
                (let ((org-options (literate-read-org-options (literate-skip-comment in))))
                  (when literate-reader-debug-p
                    (message "found org elisp src block, options:%s" org-options))
                  (cond ((literate-tangel-p (getf org-options :tangle))
                         (when literate-reader-debug-p
                           (message "enter into a elisp code block"))
                         (setf *literate-read-inside-org-code-blocks* t)
                         (literate-read-datum in))
                        (t
                         (literate-read-datum in))))))
             (*literate-read-inside-org-code-blocks*
              (let ((c (literate-next in)))
                (when literate-reader-debug-p
                       (message "found #%c inside a org block" c))
                (case c
                  (?\' (list 'function (literate-read-datum in)))
                  (?\+ 
                   (let ((line (literate-skip-comment in)))
                     (when literate-reader-debug-p
                       (message "found org elisp end block:%s" line)))
                   (setf *literate-read-inside-org-code-blocks* nil))
                  ;; FIXME: Other conditions?
                  (t (literate-orig-read in)))))
             (t
              (let ((line (literate-skip-comment in)))
                (when literate-reader-debug-p
                  (message "ignore line %s" line)))
              (literate-read-datum in))))
      ((eq ch ?\;)
       (literate-skip-comment in)
       (literate-read-datum in))
      ((eq ch ?\")
       (literate-read-string in))
      ((eq ch ?\?)
       (literate-read-char in))
      ((eq ch ?\()
       (literate-next in)
       (literate-read-list in ?\)))
      ((eq ch ?\[)
       (literate-next in)
       (apply #'vector (literate-read-list in ?\] t)))
      ((eq ch ?\')
       (literate-next in)
       (list 'quote (literate-read-datum in)))
      ((eq ch ?\`)
       (literate-next in)
       (list '\` (literate-read-datum in)))
      ((eq ch ?\,)
       (literate-next in)
       (cond
         ((eq (literate-peek in) ?\@)
          (literate-next in)
          (list '\,@ (literate-read-datum in)))
         (t
          (list '\, (literate-read-datum in)))))
      (t
       (literate-read-symbol in)))))

(defun literate-read (&optional in)
  (if (and load-file-name
          (string-match "\\.org$" load-file-name))
    (let ((*literate-read-filename* (literate-get-filename)))
      (literate-read-datum (literate-make-stream in)))
    (funcall *literate-orig-read* in)))

(defun literate-read-from-string (str &optional start end)
  (let ((*literate-read-filename* (literate-get-filename)))
    (let* ((stream (literate-make-stream
                    (substring-no-properties str start end)))
           (token (literate-read-datum stream)))
      (cons token (+ (or start 0)
                     (funcall stream :pos))))))

(defun literate-get-filename ()
  (or *literate-read-filename*
      load-file-name
      (and (boundp 'byte-compile-current-file) byte-compile-current-file)
      (and (boundp 'byte-compile-dest-file) byte-compile-dest-file)
      (buffer-file-name (current-buffer))
      (buffer-name (current-buffer))))

(defun literate-load (path)
  (let ((load-read-function (symbol-function 'literate-read))
        (*literate-read-inside-org-code-blocks* nil))
  (load path)))

(provide 'literate-reader)
