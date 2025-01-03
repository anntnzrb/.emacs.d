;;; annt-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Language Server Protocol related configurations.

;;; Code:

;;; Eglot
;; 'markdown-mode' is a soft dependency due to servers reporting docs
;; in markdown format

(use-package eglot
  :functions (eglot-ensure)
  :commands (eglot)
  :custom
  (eglot-sync-connect nil)
  (eglot-autoshutdown t))

(provide 'annt-lsp)
;;; annt-lsp.el ends here
