(use-package consult
  :ensure t
  :bind
  (:map goto-map
	    ("i"   . consult-imenu)
	    ("g"   . consult-goto-line)
	    ("M-g" . consult-goto-line)
	    ("o"   . consult-outline))
  (:map search-map
	    ("@"   . consult-kmacro)
	    ("d"   . consult-fd)
	    ("g"   . consult-ripgrep)
	    ("s"   . consult-line)
	    ("M-s" . consult-line)
        ("i" . consult-info))
  (:map ctl-x-map
	    ("b" . consult-buffer)

        ;; ctl-x-4-map
	    ("4 b" . consult-buffer-other-window)

        ;; ctl-x-r-map
	    ("r b" . consult-bookmark)

        ;; project map
	    ("p b" . consult-project-buffer))
  (:map minibuffer-local-map
        ("M-r" . consult-history))
  (:map annt-prefix-map
        ("a t t" . consult-theme)))

(use-package magit
  :ensure t
  :config
  (setopt magit-define-global-key-bindings nil)

  :bind
  (:map annt-prefix-map
	    ("g g" . magit-status)))

(use-package project
  :config
  (setopt project-kill-buffers-display-buffer-list t))

(provide 'annt-mod-file)
