(use-package! dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>"     . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)))

(use-package! nerd-icons-dired
  :after dired
  :init
  (add-hook! dired-mode-hook '(nerd-icons-dired-mode)))
