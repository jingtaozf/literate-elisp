;;; -*- lexical-binding: t -*-
;;; literate-reader.el --- A customizable Lisp reader for Emacs org file.

(defvar *literate-read-filename* nil
  "This dynamic variable will be bound by our read functions
while parsing is in progress.  It'll contain the value of
`load-file-name', or the name of the current buffer if it doesn't
have an associated file.")

(defvar *literate-read-inside-org-code-blocks* nil
  "This dynamic variable will be bound by our read functions
while parsing is in progress.  It'll indicate whether we are inside a elisp code blocks or not.")

(defvar literate-reader-debug-p nil)

;; the following method is from https://github.com/mishoo/elisp-reader.el
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

(defun literate-whitespace? (ch)
  "Tests if the given character is whitespace (XXX actually not
all Unicode whitespace chars are handled; I'm not even sure that
would be correct)."
  (memq ch '(?  ?\t ?\n ?\f ?\r #xa0)))

(defun literate-skip-whitespace (in)
  "Skip whitespace in the given stream."
  (literate-read-while in #'literate-whitespace?))

(defun literate-skip-comment (in)
  "Skip over a comment (move to end-of-line)."
  (literate-read-while in (lambda (ch)
                      (not (eq ch ?\n)))))

(defvar org-elisp-begin-src-id "#+BEGIN_SRC elisp")
(defvar org-elisp-end-src-id "#+END_SRC")
(defun literate-read-org-options (options)
  (loop for token in (split-string options)
        collect (intern token)))

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
                  (?\+ 
                   (let ((line (literate-skip-comment in)))
                     (when literate-reader-debug-p
                       (message "found org elisp end block:%s" line)))
                   (setf *literate-read-inside-org-code-blocks* nil))
                  (t (read in)))))
             (t
              (read in))))
      ((eq ch ?\;)
       (literate-skip-comment in)
       (literate-read-datum in))
      (t
       (read in)))))

(defun literate-read (&optional in)
  (if (and load-file-name
          (string-match "\\.org\\'" load-file-name))
    (let ((*literate-read-filename* (literate-get-filename)))
      (literate-read-datum (literate-make-stream in)))
    (read in)))

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

(defun literate-load-file (file)
  "Load the Lisp file named FILE."
  ;; This is a case where .elc and .so/.dll make a lot of sense.
  (interactive (list (read-file-name "Load org file: " nil nil 'lambda)))
  (literate-load (expand-file-name file)))

(provide 'literate-reader)
