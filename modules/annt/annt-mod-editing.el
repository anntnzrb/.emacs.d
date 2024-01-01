(use-package emacs
  :config
  (setq-default tab-width 4)

  ;; do not use tabs
  (setq-default indent-tabs-mode nil)

  ;; indent behaviour
  (setopt tab-always-indent 'complete)

  ;; req. `tab-always-indent'
  (setopt tab-first-completion 'word-or-paren-or-punct))

(use-package emacs
  :config
  (setopt fill-column 80)
  (setopt sentence-end-double-space nil)

  ;; sentences MUST end with a period
  (setopt sentence-end-without-period nil)

  :bind
  (:map ctl-x-map
	("f" . nil)))

(use-package emacs
  :init
  (add-hook! emacs-startup-hook '(show-paren-mode))
  :config
  (setopt show-paren-style 'mixed)
  (setopt show-paren-when-point-inside-paren nil)
  (setopt show-paren-context-when-offscreen 'child-frame))

(provide 'annt-mod-editing)
