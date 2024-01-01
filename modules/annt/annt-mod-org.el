(use-package org
  :config
  (setopt org-src-window-setup       'current-window)
  (setopt org-src-fontify-natively   t)
  (setopt org-confirm-babel-evaluate nil)

  ;; identation
  (setopt org-src-preserve-indentation     t)
  (setopt org-edit-src-content-indentation 0)

  ;; use major mode
  (setopt org-src-tab-acts-natively t))

(provide 'annt-mod-org)
