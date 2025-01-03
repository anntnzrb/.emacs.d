;;; annt-edit.el --- Editing -*- lexical-binding: t; -*-

;;; Commentary:

;; Editing configurations.

;;; Code:

(use-package emacs
  :custom
  (tab-first-completion 'word-or-paren-or-punct) ;; emacs27+
  (tab-width 2)
  (indent-tabs-mode nil))

;;; Vi emulation
;; vim, keybinds, evil

(use-package evil
  :disabled t
  :demand t
  :hook (emacs-startup . evil-mode)
  :custom
  (evil-echo-state nil)
  (evil-want-fine-undo t)
  (evil-overriding-maps nil)
  (evil-want-keybinding nil)
  (evil-want-minibuffer nil)
  (evil-undo-system 'undo-redo) ;; native (emacs28+)

  :bind
  (:map evil-motion-state-map
        ("SPC" . nil)
        ("C-z" . nil)))

(use-package evil-collection
  :demand t
  :after evil
  :config (evil-collection-init)
  :custom
  (evil-collection-want-unimpaired-p nil))

(provide 'annt-edit)
;;; annt-edit.el ends here
