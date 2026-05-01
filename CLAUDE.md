# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **literate-elisp**, an Emacs Lisp library that enables literate programming in Emacs Lisp by allowing Emacs to load Org files directly as Lisp source files. The primary source code is maintained in Org files, and the `.el` files are generated from them.

## Architecture

### Core Files
- **literate-elisp.org** - Master source file containing the implementation and documentation
- **literate-elisp.el** - Generated/tangled Emacs Lisp code (DO NOT EDIT DIRECTLY)
- **readme.org** - Documentation and demo code showcasing library usage

### Literate Programming Workflow
The codebase follows a literate programming methodology where:
1. All code changes should be made to `.org` files
2. Generated `.el` files are auto-generated and should not be edited
3. Code blocks in org files use `#+BEGIN_SRC elisp` and `#+END_SRC` delimiters
4. The `:load` header argument controls whether code blocks are loaded (yes/no/test)

## Common Development Tasks

### Running Tests
Tests use the ERT (Emacs Regression Test) framework:
```bash
# Run all tests
emacs -Q --batch -L . \
  --load literate-elisp \
  --eval "(setf literate-elisp-test-p t)" \
  --funcall literate-elisp-batch-load literate-elisp.org \
  --funcall literate-elisp-batch-load readme.org \
  --funcall ert-run-tests-batch-and-exit
```

### Loading Org Files
```elisp
;; Load the library
(load "literate-elisp.el")

;; Load an org file directly
(literate-elisp-load "file.org")

;; Interactive loading
M-x literate-elisp-load-file

;; Batch loading
(literate-elisp-batch-load)
```

### Byte Compilation
```elisp
;; Byte compile an org file to .elc
(literate-elisp-byte-compile-file "file.org")
```

### Tangling Org to Elisp
The `.el` files are generated from `.org` files through tangling. The library provides functions to convert between formats.

## Key Functions and Variables

### Core Loading Functions
- `literate-elisp-load` - Load an org file as Elisp
- `literate-elisp-load-file` - Interactive command to load org files
- `literate-elisp-batch-load` - Load org files in batch mode
- `literate-elisp-byte-compile-file` - Byte compile org files

### Reader Implementation
- `literate-elisp-read` - Custom reader function for org syntax
- `literate-elisp-read-datum` - Read Elisp data from org blocks
- `literate-elisp-org-code-blocks-p` - Dynamic variable indicating parsing mode

### Configuration Variables
- `literate-elisp-debug-p` - Toggle debug messages
- `literate-elisp-test-p` - Control loading of test code blocks
- `literate-elisp-lang-ids` - Supported language identifiers (elisp, emacs-lisp)

## Code Block Header Arguments

The `:load` header argument controls code block loading:
- `yes` (default) - Load normally
- `no` - Ignore the code block
- `test` - Load only when `literate-elisp-test-p` is true
- Variable/function name - Load if the value is non-nil

## Compatibility Extensions

### Elisp-refs Support
```elisp
(eval-after-load "elisp-refs"
  '(advice-add 'elisp-refs--read-all-buffer-forms :around 
               #'literate-elisp-refs--read-all-buffer-forms))
```

### Helpful Library Support
```elisp
(with-eval-after-load 'helpful
  (advice-add 'helpful--find-by-macroexpanding :around 
              #'literate-elisp-helpful--find-by-macroexpanding))
```

## Development Guidelines

1. **Always edit .org files** - Never modify generated .el files directly
2. **Maintain org structure** - Preserve properties, headers, and code block organization
3. **Follow existing patterns** - Use the same code block structure and header arguments
4. **Test loading** - Ensure org files load correctly before committing
5. **Debug mode** - Enable `(setf literate-elisp-debug-p t)` for troubleshooting

## Testing Approach

Tests are defined in org files with `:load test` header argument and use ERT framework. Test blocks are only loaded when `literate-elisp-test-p` is true. CI runs tests automatically via GitHub Actions on push.

## Secrets — **NEVER hardcode**

Standard rule across all OWN repos. **NEVER** commit API keys, tokens,
private keys, `.env*` files, or paths containing `/Users/<name>` (use
`~`). Detection layers:

- `gitleaks` via `.pre-commit-config.yaml` (staged content)
- Global `commit-msg` hook at `~/projects/dummy/git-hooks/` (pasted-key
  patterns in commit messages — activate with
  `git config --global core.hooksPath ~/projects/dummy/git-hooks`)
- Manual full-history sweep before any release: `gitleaks detect --no-banner --redact`

If a secret leaks: rotate immediately (`git rebase` does not erase it
from remote history). Source: lens #10 of the AI codebase mastery
research at `~/projects/dummy/notes/ai-codebase-mastery.org`.