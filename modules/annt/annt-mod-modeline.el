(use-package keycast
  :ensure t
  :defer 2
  :config
  (setopt keycast-mode-line-format "%k%c%R")
  ;; don't hide rest of the modeline
  (setopt keycast-mode-line-remove-tail-elements nil) 

  (defconst annt--keycast-events-disabled
    '(mouse-event-p
      mouse-movement-p
      mwheel-scroll
      pixel-scroll-precision
      pixel-scroll-start-momentum)
    "List of disabled events that keycast should ignore.")

  (defconst annt--keycast-events-typing
    '(self-insert-command
      org-self-insert-command
      isearch-printing-char
      isearch-delete-char)
    "List of disabled events that keycast should interpret as typing.")

  (mapc (lambda (ev)
          (add-to-list 'keycast-substitute-alist `(,ev nil nil)))
        annt--keycast-events-disabled)

  (mapc (lambda (ev)
          (add-to-list 'keycast-substitute-alist `(,ev "." "Typing…")))
        annt--keycast-events-typing)

  (keycast-mode +1)

  :bind
  (:map annt-prefix-map
        ("m k" . keycast-mode)))

(provide 'annt-mod-modeline)
