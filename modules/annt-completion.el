;;; annt-completion.el --- Completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion related configurations.

;;; Code:

;;; Core

(use-package emacs
  :custom
  ;; TAB cycle
  (completion-cycle-threshold 3)

  ;; indentation+completion using TAB
  (tab-always-indent 'complete))

;;; Orderless

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Consult

(use-package consult
  :bind
  (:map goto-map ;; M-g
        ("i"   . consult-imenu)
        ("o"   . consult-outline))
  (:map search-map ;; M-s
        ("@"   . consult-kmacro)
        ("d"   . consult-fd)
        ("g"   . consult-ripgrep)
        ("s"   . consult-line)
        ("M-s" . consult-line)
        ("i" . consult-info))
  (:map ctl-x-map
        ("b" . consult-buffer)

        ;; ctl-x-4-map
        ("4 b" . consult-buffer-other-window)

        ;; ctl-x-r-map
        ("r b" . consult-bookmark)

        ;; project map
        ("p b" . consult-project-buffer))
  (:map minibuffer-local-map
        ("M-r" . consult-history)))

;;; Corfu

(use-package corfu
  :if (display-graphic-p)
  :custom
  (corfu-preview-current nil)
  (corfu-min-width 20)

  (corfu-popupinfo-delay '(1.25 . 0.5))
  :hook ((emacs-startup . global-corfu-mode)
         (emacs-startup . corfu-popupinfo-mode))
  :bind
  (:map corfu-map
        ("<tab>" . corfu-complete)))

(use-package nerd-icons-corfu
  :demand t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'annt-completion)
;;; annt-completion.el ends here
