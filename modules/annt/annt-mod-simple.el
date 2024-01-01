(use-package emacs
  :config

  (setopt read-answer-short t)
  (setopt use-short-answers t)

  ;; reverting/refreshing/updating
  (setopt auto-revert-verbose t)
  (global-auto-revert-mode +1))

(use-package use-package
  :config
  (setopt use-package-always-defer t)

  ;; force using the "-hook" suffix, this should be default...
  (setopt use-package-hook-name-suffix nil))

(use-package server
  :demand t
  :config
  (setopt server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(defvar-keymap annt-prefix-map
  :doc "annt's prefix keymap with multiple subkeymaps.")

(define-key global-map (kbd "C-z") annt-prefix-map)

(setopt backup-inhibited nil)
(setopt create-lockfiles nil)
(setopt make-backup-files nil)

(setopt initial-buffer-choice t)

(use-package emacs
  :config
  (setopt initial-major-mode 'text-mode)
  (setopt initial-scratch-message nil))

(use-package pixel-scroll
  :demand t
  :config
  (pixel-scroll-precision-mode +1))

(use-package help
  :config
  (setopt help-window-keep-selected t)
  (setopt help-window-select        t))

(provide 'annt-mod-simple)
