(use-package evil
  :ensure t
  :demand t
  :init
  (setopt evil-echo-state      nil)
  (setopt evil-overriding-maps nil)
  (setopt evil-want-fine-undo    t)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-minibuffer nil)

  ;; native undo-redo
  (setopt evil-undo-system 'undo-redo)

  (evil-mode +1)

  :bind
  (:map evil-motion-state-map
	("C-z" . nil)))

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (setopt evil-collection-want-unimpaired-p nil)

  (evil-collection-init))

(provide 'annt-mod-vi)
