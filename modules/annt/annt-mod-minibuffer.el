(use-package minibuffer
  :config
  (setopt minibuffer-default-prompt-format " [%s]")
  (setopt resize-mini-windows t)

  ;; save minibuffer history across sessions
  (setopt history-length t)
  (setopt history-delete-duplicates t)
  (savehist-mode +1)

  ;; hide default value when typing
  (minibuffer-electric-default-mode +1)

  ;; remove "shadow" greyed out parts
  (file-name-shadow-mode +1)

  :bind
  (:map minibuffer-mode-map
        ;; unlikely to use an actual tab character, so prefer completion
        ("M-\\" . dabbrev-expand)))

(use-package vertico
  :ensure t
  :config
  (setopt vertico-cycle    t)
  (setopt vertico-count   12)
  (setopt vertico-resize nil)
  (setopt vertico-scroll-margin (/ vertico-count 2))
  (setopt vertico-sort-function 'vertico-sort-history-length-alpha)

  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous))

  :init
  (vertico-mode +1))

(use-package marginalia
  :ensure t
  :defer 1
  :config
  (setopt marginalia-align  'left)
  (setopt marginalia-separator "    ")
  (setopt marginalia-max-relative-age 0)

  (marginalia-mode +1))

(provide 'annt-mod-minibuffer)
