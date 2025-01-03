;;; annt-prog.el --- Programming -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming related configurations.

;;; Code:

;;; Flymake

(use-package flymake
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-lighter nil))

;;; Eldoc

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)) ;; lighter

(provide 'annt-prog)
;;; annt-prog.el ends here
