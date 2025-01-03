;;; annt-project.el --- Project -*- lexical-binding: t; -*-

;;; Commentary:

;; Project related configurations.

;;; Code:

;;; Core

(use-package project
  :bind
  (:map project-prefix-map
        ("<return>"    . project-dired)
        ("<backspace>" . project-forget-project)
        ("C-g"         . keyboard-quit)
        ("ESC"         . keyboard-quit)))

(provide 'annt-project)
;;; annt-project.el ends here
