;;; annt-minibuffer.el --- Minibuffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Minibuffer related configurations.

;;; Code:

;;; Core

(use-package crm
  :custom
  ;; hide cmds in M-x which do not work in current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[SEP:%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;; Vertico

(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-scroll-margin (/ vertico-count 2))
  (vertico-sort-function 'vertico-sort-history-length-alpha))

;;; Marginalia

(use-package marginalia
  :demand t
  :after vertico
  :config (marginalia-mode +1)
  :custom
  (marginalia-align  'left)
  (marginalia-separator "    ")
  (marginalia-max-relative-age 0))

(use-package nerd-icons-completion
  :demand t
  :if (display-graphic-p)
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'annt-minibuffer)
;;; annt-minibuffer.el ends here
