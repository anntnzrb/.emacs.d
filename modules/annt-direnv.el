;;; annt-direnv.el --- Direnv -*- lexical-binding: t; -*-

;;; Commentary:

;; Direnv configurations.

;;; Code:

;;; Mode

(use-package envrc
  :hook (emacs-startup . envrc-global-mode)
  :custom
  (envrc-lighter nil))

(provide 'annt-direnv)
;;; annt-direnv.el ends here
