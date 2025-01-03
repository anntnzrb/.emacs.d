;;; annt-lang-nix.el --- Nix Language -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix Language configurations.

;;; Code:

;;; Mode

(use-package nix-ts-mode
  :mode (rx ".nix" eos))

;;; LSP

(use-package eglot
  :after nix-ts-mode
  :config
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) "nil"))
  :hook (nix-ts-mode . eglot-ensure))

(provide 'annt-lang-nix)
;;; annt-lang-nix.el ends here
