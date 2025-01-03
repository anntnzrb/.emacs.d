;;; annt-core.el --- Core -*- lexical-binding: t; -*-

;;; Commentary:

;; Core configurations.

;;; Code:

(use-package emacs
  :custom
  (read-answer-short t)
  (use-short-answers t) ;; emacs28+

  ;; reverting/refreshing/updating
  (auto-revert-verbose t)

  :config
  (global-auto-revert-mode +1))

;;; Keymaps

;; Disabled binds

(use-package emacs
  :bind
  (:map global-map
        ("s-x" . nil)  ;; kill-region
        ("s-x" . nil)  ;; kill-region
        ("C-z" . nil)  ;; suspend-frame
        ("C-s" . nil)) ;; isearch-forward
  (:map ctl-x-map
        ("f" . nil))) ;; set-fill-column

;; Main prefix (leader) key

(use-package emacs
  :bind-keymap (("S-SPC" . annt-prefix))
  :config
  (defvar-keymap annt-prefix-map
    :doc "Prefix keymap with multiple subkeymaps."
    :prefix 'annt-prefix)
  
  :bind
  (:map annt-prefix
        ("C-g" . #'keyboard-quit)
        ("ESC" . #'keyboard-quit)))

;;; use-package

(use-package use-package
  :demand t
  :custom
  (use-package-verbose nil)
  (use-package-always-defer t)
  (use-package-always-ensure nil)
  (use-package-compute-statistics t)
  (use-package-check-before-init nil) ;; performance hit
  (use-package-ignore-unknown-keywords nil))

(provide 'annt-core)
;;; annt-core.el ends here
