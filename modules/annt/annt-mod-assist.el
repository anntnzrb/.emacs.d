(use-package which-key
  :ensure t
  :defer 2
  :config
  (setopt which-key-lighter           "")
  (setopt which-key-separator     " ➡ ")
  (setopt which-key-prefix-prefix "... ")

  (setopt which-key-add-column-padding     16)
  (setopt which-key-max-display-columns     4)
  (setopt which-key-max-description-length 40)

  ;; timings
  (setopt which-key-idle-delay            0.5)
  (setopt which-key-idle-secondary-delay 0.25)

  (which-key-mode +1))

(provide 'annt-mod-assist)
