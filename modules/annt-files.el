;;; annt-files.el --- Files -*- lexical-binding: t; -*-

;;; Commentary:

;; File related configurations.

;;; Code:

;;; Backups & Lockfiles

(use-package files
  :custom
  (backup-inhibited nil)
  (create-lockfiles nil)
  (make-backup-files nil))

(provide 'annt-files)
;;; annt-files.el ends here
