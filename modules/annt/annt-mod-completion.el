(use-package nerd-icons-completion
  :ensure t
  :defer 3
  :config
  (nerd-icons-completion-mode +1))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup))

(use-package emacs
  :config
  (setopt completion-styles '(basic substring initials flex partial-completion orderless))

  ;; do not need anything case-specific most of the time (?)
  (setopt completion-ignore-case                t)
  (setopt read-buffer-completion-ignore-case    t)
  (setopt read-file-name-completion-ignore-case t))

(use-package orderless
  :ensure t
  :defer 2
  :config
  (setopt orderless-matching-styles
          '(orderless-prefixes orderless-regexp)))

(use-package corfu
  :ensure t
  :config
  (setopt corfu-auto  t)
  (setopt corfu-count 8)
  (setopt corfu-cycle t)

  (setopt corfu-min-width   20)
  (setopt corfu-auto-delay  0.1)
  (setopt corfu-auto-prefix 2)

  ;; popupinfo
  (setopt corfu-popupinfo-delay '(1.5 . 0.5))

  :bind
  (:map corfu-map
        ;; ignores completion candidates and jumping to newline
        ("M-<return>" . newline))
  :init
  (global-corfu-mode    +1)
  (corfu-popupinfo-mode +1))

(provide 'annt-mod-completion)
