;;; annt-dired.el --- Dired -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired configurations.

;;; Code:

;;; Core

(use-package dired
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . hl-line-mode)

  :custom
  (dired-recursive-copies    'always)
  (dired-recursive-deletes   'always)
  (delete-by-moving-to-trash t)

  ;; do not display available space (top)
  (dired-free-space nil)

  ;; update dired contents when directory contens change
  (dired-auto-revert-buffer #'dired-buffer-stale-p)

  ;; `man' ls for extra flags info
  (dired-listing-switches "-AGFhlv")

  ;; smart dired
  (dired-dwim-target t)

  ;; when renaming (mv) a vc-controlled file, use vc mv over traditional mv
  (dired-vc-rename-file t)

  ;; offer creating specified directory paths if missing
  (dired-create-destination-dirs 'always)
  (dired-create-destination-dirs-on-trailing-dirsep t)

  :bind
  (:map dired-mode-map
        ("C-+" . dired-create-empty-file)))

;;; wdired

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;;; subtree

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>"     . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)))

;;; Icons

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'annt-dired)
;;; annt-dired.el ends here
