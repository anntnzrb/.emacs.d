;;; annt-lang-elisp.el --- Emacs Lisp Language -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Lisp configurations.

;;; Code:

(use-package elisp-mode
  :hook (emacs-lisp-mode . flymake-mode))

(provide 'annt-lang-elisp)
;;; annt-lang-elisp.el ends here
