;;; annt-lang-gleam.el --- Gleam Language -*- lexical-binding: t; -*-

;;; Commentary:

;; Gleam Language configurations.

;;; Code:

;;; Mode

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

;;; LSP

(use-package eglot
  :after gleam-ts-mode
  :config
  (add-to-list 'eglot-server-programs '(gleam-ts-mode "gleam" "lsp"))
  :hook (gleam-ts-mode . eglot-ensure))

(provide 'annt-lang-gleam)
;;; annt-lang-gleam.el ends here
