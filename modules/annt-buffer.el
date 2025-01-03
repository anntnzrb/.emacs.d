;;; annt-buffer.el --- Buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer related configurations.

;;; Code:

;;; Core

(use-package emacs
  ;; remap traditional `list-buffers'
  :bind ([remap kill-buffer] . kill-buffer-and-window))

;;; Keymap

(use-package emacs
  :config
  (defvar-keymap annt-prefix-buffer-map
    :doc "Prefix keymap for buffers."
    :prefix 'annt-prefix-buffer)
  
  ;; prefix bindings
  :bind
  (:map annt-prefix ("b" . 'annt-prefix-buffer))
  (:map annt-prefix-buffer
        ("b" . #'consult-buffer)
        ("i" . #'ibuffer)
        ("k" . #'kill-buffer-and-window)))

;; Initial Buffer

(use-package emacs
  :custom
  (initial-buffer-choice t) ;; *scratch* buffer
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil))

;; ibuffer

(use-package ibuffer
  ;; remap traditional `list-buffers'
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-view-ibuffer nil)
  (ibuffer-case-fold-search t)
  (ibuffer-use-other-window nil)
  (ibuffer-always-compile-formats t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-reversep nil)
  (ibuffer-default-sorting-mode 'recency))

(provide 'annt-buffer)
;;; annt-buffer.el ends here
