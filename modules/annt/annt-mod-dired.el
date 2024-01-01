(use-package dired
  :init
  (add-hook! dired-mode-hook '(dired-hide-details-mode hl-line-mode))

  :config
  (setopt dired-recursive-copies    'always)
  (setopt dired-recursive-deletes   'always)
  (setopt delete-by-moving-to-trash t)

  ;; do not display available space (top)
  (setopt dired-free-space nil)

  ;; update dired contents when directory contens change
  (setopt dired-auto-revert-buffer #'dired-directory-changed-p)

  ;; `man' ls for extra flags info 
  (setopt dired-listing-switches
          "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; smart dired
  (setopt dired-dwim-target t)

  ;; when renaming (mv) a vc-controlled file, use vc mv over traditional mv
  (setopt dired-vc-rename-file t)

  ;; offer creating specified directory paths if missing
  (setopt dired-create-destination-dirs 'always)
  (setopt dired-create-destination-dirs-on-trailing-dirsep t)

  :bind
  (:map dired-mode-map
        ("C-+" . dired-create-empty-file)))

(use-package wdired
  :after dired
  :config
  (setopt wdired-allow-to-change-permissions t)
  (setopt wdired-create-parent-directories   t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>"     . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)))

(use-package nerd-icons-dired
  :ensure t
  :after dired
  :init
  (add-hook! dired-mode-hook '(nerd-icons-dired-mode)))

(provide 'annt-mod-dired)
