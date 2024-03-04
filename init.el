;; GNU Emacs early initialisation -*- lexical-binding: t; -*-

;; GNU Emacs initialisation -*- lexical-binding: t; -*-

(defconst annt/init--config-literate
  (expand-file-name "readme.org" user-emacs-directory)
  "File path of the literate configuration file.")

(defun annt/init--tangle-literate-config ()
  "Prompt to tangle literate configuration file."
  (interactive)
  (when (and (equal (buffer-file-name) annt/init--config-literate)
             (y-or-n-p "Save and tangle before exiting? "))
    (save-buffer)
    (org-babel-tangle)))

(add-hook 'kill-emacs-hook #'annt/init--tangle-literate-config)

(defmacro annt/helpers--add-hook (hooks functions &optional depth local)
  "Replacement and wrapper for `add-hook'.
Add N FUNCTIONS to M HOOKS. Both optional DEPTH and LOCAL arguments are passed
to `add-hook'.

NOTE: The mode hook should not be quoted."
  (let ((hooksp (listp hooks))
        (fnsp   (listp functions)))
    (cond
     ;; both HOOKS and FUNCTIONS are lists
     ((and hooksp fnsp)
      `(mapc (lambda (hk)
               (mapc (lambda (fn)
                       (add-hook 'hk fn ,depth ,local))
                     ,functions))
             ,hooks))

     ;; only HOOKS is a list
     (hooksp
      `(mapc (lambda (hk)
               (add-hook 'hk ,functions ,depth ,local))
             ,hooks))

     ;; only FUNCTIONS is a list
     (fnsp
      `(mapc (lambda (fn)
               (add-hook ',hooks fn ,depth ,local))
             ,functions))

     ;; neither HOOKS nor FUNCTIONS is a list
     ((not (or hooksp fnsp))
      `(add-hook ',hooks ,functions ,depth ,local))

     ;; fallback to error message
     (t `(error "Invalid arguments: hooks and functions should be either a symbol or a list")))))

(defalias 'add-hook! #'annt/helpers--add-hook)

;;;###autoload
(defun append-path (path)
  "Add the specified PATH to the `exec-path' and 'PATH' environment variable."
  (let ((expanded-path (expand-file-name path)))
    (if (file-directory-p expanded-path)
        (progn
          (add-to-list 'exec-path expanded-path)
          (setenv "PATH" (concat (getenv "PATH") path-separator expanded-path)))
      (warn "Warning: %s is not a directory" expanded-path))))

;;;###autoload
(defun termbin-upload (start end)
  "Uploads region between START and END to termbin.com and copies resulting URL.

This function operates asynchronously."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (proc (make-process :name "termbin-upload"
                             :command '("nc" "termbin.com" "9999")
                             :connection-type 'pipe
                             :buffer nil
                             :filter (lambda (_ string)
                                       (let ((trimmed-string (string-trim-right string "[\n\0]+")))
                                         (message "Uploaded and copied URL: %s" trimmed-string)
                                         (kill-new trimmed-string))))))
    (message "Uploading...")
    (process-send-string proc content)
    (process-send-eof proc)))

(use-package cus-edit
  :config
  (setopt custom-buffer-done-kill t))

(setopt custom-file (make-temp-file "emacs-custom-file-"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use bleeding-edge straight.el
(setopt straight-repository-branch "master")

;; do not fully clone repos, just not needed
(setopt straight-vc-git-default-clone-depth 1)

;; always prefer native compilation
(setopt straight-disable-compile nil)
(setopt straight-disable-native-compile nil)

(use-package emacs
  :config

  (setopt read-answer-short t)
  (setopt use-short-answers t)

  ;; reverting/refreshing/updating
  (setopt auto-revert-verbose t)
  (global-auto-revert-mode +1))

(use-package use-package
  :config
  ;; always lazy load packages, unless instructed otherwise; use `:demand' to do
  ;; so, for extra info, see:
  ;; <https://github.com/jwiegley/use-package#notes-about-lazy-loading>
  (setopt use-package-verbose           nil)
  (setopt use-package-always-defer        t)
  (setopt use-package-check-before-init nil) ;; performance hit
  (setopt use-package-compute-statistics  t)
  (setopt use-package-ignore-unknown-keywords nil)

  ;; force using the "-hook" suffix, this should be default...
  (setopt use-package-hook-name-suffix nil)

  (defmacro annt/package--use-package-external-package (package &rest body)
    "Wrapper around `use-package' with external package integration; `straight.el'
  in this case.
  PACKAGE and BODY are as in `use-package.'"
    (declare (indent 1))
    `(use-package ,package :straight t ,@body))

  (defalias 'use-package! #'annt/package--use-package-external-package))

(use-package server
  :demand t
  :config
  (setopt server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package keymap
  :config
  (defvar-keymap annt/keys--leader-prefix-map
    :doc "annt's leader keymap.")

  (defun annt/misc--goto-emacs-config-directory ()
    "Opens one's Emacs configuration directory."
    (interactive)
    (find-file user-emacs-directory))

  (defun annt/misc--goto-emacs-config-file ()
    "Opens one's Emacs configuration file."
    (interactive)
    (find-file (expand-file-name "readme.org" user-emacs-directory)))

  (keymap-set global-map "C-z" annt/keys--leader-prefix-map)

  :bind
  (:map annt/keys--leader-prefix-map
        ;; system
        ("/ r" . restart-emacs)
        ("/ q" . save-buffers-kill-terminal)

        ;; buffer
        ("b r" . restart-emacs)

        ;; private configuration
        ("f p d" . annt/misc--goto-emacs-config-directory)
        ("f p p" . annt/misc--goto-emacs-config-file)))

(setopt backup-inhibited nil)
(setopt create-lockfiles nil)
(setopt make-backup-files nil)

(use-package pixel-scroll
  :demand t
  :config
  (pixel-scroll-precision-mode +1))

(use-package help
  :config
  (setopt help-window-keep-selected t)
  (setopt help-window-select        t))

(use-package! helpful
  :bind
  (:map help-map
        ("o" . helpful-symbol)
        ("f" . helpful-callable)
        ("v" . helpful-variable)
        ("k" . helpful-key)
        ("x" . helpful-command)))

(use-package! which-key
  :defer 2
  :config
  (setopt which-key-lighter           "")
  (setopt which-key-separator     " ➡ ")
  (setopt which-key-prefix-prefix "... ")

  (setopt which-key-add-column-padding     16)
  (setopt which-key-max-display-columns     4)
  (setopt which-key-max-description-length 40)

  ;; timings
  (setopt which-key-idle-delay            0.5)
  (setopt which-key-idle-secondary-delay 0.25)

  (which-key-mode +1))

(use-package emacs
  :config
  (setopt initial-buffer-choice t) ;; *scratch* buffer
  (setopt initial-major-mode 'text-mode)
  (setopt initial-scratch-message nil)

  :bind
  (:map ctl-x-map
        ("k" . kill-current-buffer)))

(use-package emacs
  :config
  (setopt tab-width 4)

  ;; do not use tabs
  (setopt indent-tabs-mode nil)

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

(use-package! evil
  :demand t
  :init
  (setopt evil-echo-state      nil)
  (setopt evil-overriding-maps nil)
  (setopt evil-want-fine-undo    t)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-minibuffer nil)

  ;; native undo-redo (emacs28+)
  (setopt evil-undo-system 'undo-redo)

  (evil-mode +1)

  :bind
  (:map evil-motion-state-map
        ("C-z" . nil)))

(use-package! evil-collection
  :demand t
  :after evil
  :config
  (setopt evil-collection-want-unimpaired-p nil)

  (evil-collection-init))

(use-package emacs
  :init
  (add-hook! emacs-startup-hook '(show-paren-mode))
  :config
  (setopt show-paren-style 'mixed)
  (setopt show-paren-when-point-inside-paren nil)
  (setopt show-paren-context-when-offscreen 'child-frame))

(use-package emacs
  :config
  ;; FIXME: `setopt' fails
  (setq completion-styles '(basic substring initials flex partial-completion orderless))

  ;; do not need anything case-specific most of the time (?)
  (setopt completion-ignore-case                t)
  (setopt read-buffer-completion-ignore-case    t)
  (setopt read-file-name-completion-ignore-case t))

(use-package! orderless
  :defer 2
  :config
  (setopt orderless-matching-styles
          '(orderless-prefixes orderless-regexp)))

(use-package! doom-modeline
  :disabled t
  :demand t
  :config
  (setopt doom-modeline-icon t)

  ;; buffer
  (setopt doom-modeline-buffer-name                    t)
  (setopt doom-modeline-buffer-encoding                t)
  (setopt doom-modeline-buffer-state-icon              t)
  (setopt doom-modeline-buffer-file-name-style         'truncate-with-project)
  (setopt doom-modeline-buffer-modification-icon       t)
  (setopt doom-modeline-highlight-modified-buffer-name t)

  (setopt doom-modeline-column-zero-based nil)
  (setopt doom-modeline-total-line-number t)

  ;; modal
  (setopt doom-modeline-modal                      t)
  (setopt doom-modeline-modal-icon                 t)
  (setopt doom-modeline-modal-modern-icon          t)
  (setopt doom-modeline-always-show-macro-register t)

  ;; modes
  (setopt doom-modeline-major-mode-icon       t)
  (setopt doom-modeline-major-mode-color-icon t)
  (setopt doom-modeline-minor-modes           nil)

  (add-hook 'emacs-startup-hook  #'doom-modeline-mode))

(use-package! keycast
  :defer 2
  :config
  (setopt keycast-mode-line-format "%k%c%R")
  ;; don't hide rest of the modeline
  (setopt keycast-mode-line-remove-tail-elements nil) 

  (defconst annt/keycast--events-disabled
    '(mouse-event-p
      mouse-movement-p
      mwheel-scroll
      pixel-scroll-precision
      pixel-scroll-start-momentum)
    "List of disabled events that keycast should ignore.")

  (defconst annt/keycast--events-typing
    '(self-insert-command
      org-self-insert-command
      isearch-printing-char
      isearch-delete-char)
    "List of disabled events that keycast should interpret as typing.")

  (mapc (lambda (ev)
          (add-to-list 'keycast-substitute-alist `(,ev nil nil)))
        annt/keycast--events-disabled)

  (mapc (lambda (ev)
          (add-to-list 'keycast-substitute-alist `(,ev "." "Typing…")))
        annt/keycast--events-typing))

(use-package minibuffer
  :config
  (setopt minibuffer-default-prompt-format " [%s]")
  (setopt resize-mini-windows t)

  ;; save minibuffer history across sessions
  (setopt history-length t)
  (setopt history-delete-duplicates t)
  (savehist-mode +1)

  ;; hide default value when typing
  (minibuffer-electric-default-mode +1)

  ;; remove "shadow" greyed out parts
  (file-name-shadow-mode +1)

  :bind
  (:map minibuffer-mode-map
        ;; unlikely to use an actual tab character, so prefer completion
        ("M-\\" . dabbrev-expand)))

(use-package! vertico
  :config
  (setopt vertico-cycle    t)
  (setopt vertico-count   12)
  (setopt vertico-resize nil)
  (setopt vertico-scroll-margin (/ vertico-count 2))
  (setopt vertico-sort-function 'vertico-sort-history-length-alpha)

  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous))

  :init
  (vertico-mode +1))

(use-package! marginalia
  :defer 1
  :config
  (setopt marginalia-align  'left)
  (setopt marginalia-separator "    ")
  (setopt marginalia-max-relative-age 0)

  (marginalia-mode +1))

(use-package! nerd-icons-completion
  :defer 3
  :config
  (nerd-icons-completion-mode +1))

(use-package! nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup))

(use-package emacs
  :after custom
  :config
  (let ((time (string-to-number (format-time-string "%H"))))
    (defconst annt/themes--time-day-p (and (> time 9) (< time 18))
      "Non-nil if the time of the day is daylight.")
    (defconst annt/themes--time-night-p (not annt/themes--time-day-p)
      "Non-nil if the time of the day is the dark night.")))

(use-package modus-themes
  :demand t
  :init
  (load-theme 'modus-operandi t t) ;; load; no enabling yet
  :config
  (setq modus-themes-links         '(background italic))
  (setq modus-themes-region        '(no-extend bg-only accented))
  (setq modus-themes-syntax        '(green-strings yellow-comments))
  (setq modus-themes-mode-line     '(accented borderless))
  (setq modus-themes-org-blocks    'greyscale)
  (setq modus-themes-paren-match   '(bold underline))
  (setq modus-themes-bold-constructs   t)
  (setq modus-themes-italic-constructs t)

  ;; use operandi for daylight
  (modus-themes-load-operandi)

  :bind
  (:map global-map
        ("M-<f5>" . modus-themes-toggle)))

(use-package! ef-themes
  :disabled t
  :demand t
  :config
  ;; use any dark theme for night
  (when annt/themes--time-night-p
    (ef-themes-load-random 'dark)))

(use-package! fontaine
  :init
  (setopt text-scale-remap-header-line t)

  (setopt fontaine-presets
          '(;
            (iosevka-mononoki
             :default-family "mononoki"
             :variable-pitch-family "Iosevka Comfy Motion"
             :variable-pitch-weight semilight)

            (mononoki-iosevka
             :default-family "Iosevka Comfy Motion"
             :variable-pitch-family "mononoki"
             :variable-pitch-weight semilight)

            ;; fallback
            (default)
            (t
             :default-family nil
             :default-weight regular
             :default-height 130

             :variable-pitch-family nil
             :variable-pitch-weight nil
             :variable-pitch-height 1.0

             :fixed-pitch-family nil
             :fixed-pitch-weight nil
             :fixed-pitch-height 1.0

             :fixed-pitch-serif-family nil
             :fixed-pitch-serif-weight nil
             :fixed-pitch-serif-height 1.0

             :bold-family nil
             :bold-weight bold
             :italic-family nil
             :italic-slant italic

             :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))

  (add-hook 'kill-emacs-hook        #'fontaine-store-latest-preset)
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset))

(use-package! nerd-icons :defer 3)

(use-package org
  :config
  (setopt org-src-window-setup       'current-window)
  (setopt org-src-fontify-natively   t)
  (setopt org-confirm-babel-evaluate nil)

  ;; identation
  (setopt org-src-preserve-indentation     t)
  (setopt org-edit-src-content-indentation 0)

  ;; use major mode's tabs for src blocks
  (setopt org-src-tab-acts-natively t))

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

(use-package! consult
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
  (:map annt/keys--leader-prefix-map
        ("b b" . consult-buffer))
  (:map minibuffer-local-map
        ("M-r" . consult-history)))

(use-package! magit
  :config
  (setopt magit-define-global-key-bindings nil)

  :bind
  (:map global-map
        ("C-c g g" . magit-status)))

(use-package project
  :config
  (setopt project-kill-buffers-display-buffer-list t))

(use-package! nix-mode
  :mode "\\.nix\\'"
  :init
  ;; treat flake.lock files as json
  (add-to-list 'auto-mode-alist (cons "/flake\\.lock\\'" 'js-mode))
  :config
  (add-hook 'nix-mode-local-vars-hook #'tree-sitter! 'append))
