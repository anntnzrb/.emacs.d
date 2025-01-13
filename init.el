;;; init.el --- Initialization file of GNU Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020-2024 anntnzrb

;; Author: anntnzrb <anntnzrb@proton.me>
;; Keywords: initialization

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialization related configurations.

;;; Code:

;;; use-package

(use-package use-package
  :custom
  (use-package-verbose nil)
  (use-package-always-defer t)
  (use-package-always-ensure nil)
  (use-package-hook-name-suffix nil)
  (use-package-compute-statistics t)
  (use-package-check-before-init nil) ;; performance hit
  (use-package-ignore-unknown-keywords nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pixel Scroll

(use-package pixel-scroll
  :custom
  (pixel-scroll-precision-mode t))

;;; Which-Key

(use-package which-key
  :custom
  ;; suppress minor mode message
  (which-key-mode t)
  (which-key-lighter "")
  (which-key-show-early-on-C-h t))

;;; Theme

(use-package ef-themes
  :demand t
  :config
  ;; randomly choose a theme
  (ef-themes-load-random 'light)
  
  :bind
  (:map global-map
	("C-c t r" . ef-themes-rotate)
	("C-c t t" . ef-themes-select)))

;;; Font
;; cf. https://youtu.be/qR8JRYr4BKE

;; `fixed-pitch': monospace
;; `variable-pitch': proportional space

(use-package fontaine
  :if (display-graphic-p) ;; only if running via GUI Emacs
  :bind (:map global-map
	      ("C-c f" . fontaine-set-preset))
  
  :custom
  (fontaine-mode t)
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets
   '(;
     (iosevka
      :default-family "Iosevka Comfy Motion"
      :default-height 140
      
      :fixed-pitch-family "Iosevka Comfy Wide Motion"
      :variable-pitch-family "Iosevka Comfy Wide Motion Duo")

     (medium
      :default-height 140)

     ;; fallback
     (default)
     (t
      :default-family "Iosevka Comfy Motion"
      :default-height 120)))

  ;; some misc font settings
  (x-underline-at-descent-line nil)
  (text-scale-remap-header-line t) ;; emacs28
  :hook
  (emacs-startup-hook . (lambda ()
			  "Set last preset or fall back to desired style from `fontaine-presets'."
			  (fontaine-set-preset
			   (or (fontaine-restore-latest-preset) 'default)))))

(use-package fontaine
  :after pulsar
  :hook (fontaine-set-preset-hook . pulsar-pulse-line))

;;; Highlight Line

(use-package pulsar
  :hook (minibuffer-setup-hook . pulsar-pulse-line)
  :custom
  (pulsar-global-mode t)
  (pulsar-delay 0.05))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Direnv

(use-package envrc
  :hook (emacs-startup-hook . envrc-global-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modeline
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :custom
  (column-number-mode t)
  (size-indication-mode nil)
  ;; `display-line-numbers' does this already
  (line-number-mode nil))

;;; Time

(use-package time
  :custom
  (display-time-mode t)
  (display-time-format " %a %e %b, %H:%M ")
  (display-time-interval 60)
  (display-time-default-load-average nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :config
  (defun annt-simple-kill-region-dwim ()
    "Kill the region if active, otherwise kill the whole line."
    (interactive)
    (if (use-region-p)
	(kill-region (region-beginning) (region-end))
      (kill-whole-line)))

  (defun annt-simple-newline ()
    "Insert newline, regardless of point."
    (interactive)
    (end-of-line)
    (newline))
  
  :custom
  ;;; Visual-Lines / Logical-Lines
  ;; treat visual-lines as logical-lines
  (global-hl-line-mode t)
  (line-move-visual nil)

  :bind
  (:map global-map
	([remap kill-region] . annt-simple-kill-region-dwim)
	("S-<return>" . annt-simple-newline)))

;;; Evil Mode (Vim layer)

(use-package simple
  :config (evil-mode +1))

(use-package evil
  :custom
  (evil-echo-state nil)
  (evil-want-fine-undo t)
  (evil-overriding-maps nil)
  (evil-want-keybinding nil)
  (evil-want-minibuffer nil)
  (evil-undo-system 'undo-redo) ;; emacs28 (native)

  :bind
  (:map evil-motion-state-map
	;; replace search with `consult-line'
 	([remap evil-search-forward] . consult-line)
	("C-r" . evil-redo)
	("C-v" . evil-visual-block))

  :config
  ;; see (find-library "evil-maps")
  (defun disable-evil-keys (prefix modes)
    "Disable Evil bindings for keys with the specified PREFIX in the given MODES.
PREFIX can be 'C-', 'M-', or any other valid key prefix.
MODES is a list of Evil state keymaps (e.g., '(evil-normal-state-map evil-insert-state-map))."
    (let ((keys (mapcar (lambda (char) (kbd (concat prefix (char-to-string char))))
			(append (number-sequence ?a ?z) (number-sequence ?0 ?9)))))
      (dolist (mode-map modes)
	(dolist (key keys)
          (define-key (symbol-value mode-map) key nil)))))

  (disable-evil-keys "C-"   '(evil-normal-state-map evil-insert-state-map))
  (disable-evil-keys "M-"   '(evil-normal-state-map evil-insert-state-map))
  (disable-evil-keys "C-M-" '(evil-normal-state-map evil-insert-state-map)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :bind
  (:map ctl-x-map
	("f". nil)) ;; `set-fill-column'
  (:map global-map
	;; these 2 are replaced by `consult-line'
	("C-s" . nil)   ;; `isearch-forward'
	("C-r" . nil))) ;; `isearch-backward'

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :custom

  ;;; Auto Revert
  
  (global-auto-revert-mode t)
  (auto-revert-verbose nil)
  (auto-revert-avoid-polling t)

  ;;; Lines Numbers
  
  (global-display-line-numbers-mode 't)
  (display-line-numbers-type t) ;; `t' for absolute
  (display-line-numbers-width-start t)
  
  :bind
  (:map global-map
	;; `ibuffer' > `list-buffers'
	([remap list-buffers] . ibuffer)))

;;; IBuffer

(use-package ibuffer
  :config
  ;; use human-radable sizes
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

;;; Consult

(use-package consult
  :bind
  (:map global-map
	([remap switch-to-buffer] . consult-buffer)
	;; this overwrites `tab-to-tab-stop' which is some legacy thing
	("M-i" . consult-imenu))
  (:map goto-map
	("i" . nil)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :bind
  (:map global-map
	;; `other-window' is a commonly used cmd and "M-o" is dissocuppied
	("C-x o" . nil)
	("M-o"   . other-window)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package frame
  :custom
  (frame-title-format
   (format "%%b - GNU Emacs @ %s" (system-name))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Completion Styles:
;; `partial-completion' allows `~/.l/s/f' -> `~/.local/share/fonts'

(use-package emacs
  :custom
  (tab-always-indent 'complete))

;;; Orderless

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless))))))

;;; Vertico

(use-package vertico
  :custom
  
  (vertico-mode t)
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-mouse-mode t)
  (vertico-scroll-margin (/ vertico-count 2))
  (vertico-sort-function 'vertico-sort-history-length-alpha))

(use-package rfn-eshadow
  :after vertico
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

;;; Marginalia

(use-package marginalia
  :custom
  (marginalia-mode t)
  (marginalia-align  'left)
  (marginalia-separator "    ")
  (marginalia-max-relative-age 0))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search & Replace
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :bind
  (:map search-map ;; M-s
	("s" . consult-line)
	("g" . consult-ripgrep)))

(use-package consult
  :bind
  (:map goto-map
	("o" . consult-outline)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :custom

  ;;; Auto Saving

  (auto-save-interval 60) ;; every 1 min
  (auto-save-no-message t))

(use-package consult
  :bind
  (:map search-map ;; M-s
	("f" . consult-fd)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vc-hooks
  ;; `vc-hooks' is the always-loaded portion of VC

  :custom
  ;; keep the vc backends I use; disable the rest
  (vc-handled-backends '(Git)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cf. https://youtu.be/L_4pLN0gXGI?si=X8ItW3E_ypcV9eyS

(use-package dired
  :hook
  (dired-mode-hook . hl-line-mode)
  (dired-mode-hook . dired-hide-details-mode)

  :custom
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  
  ;; do not display available space (top)
  (dired-free-space nil)

  ;; see `insert-directory-program'
  (insert-directory-program "gls") ;; GNU ls
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; see `global-auto-revert-mode'
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  
  :bind
  (:map global-map
	;; I consider these useless as one can `dired' via `find-file'
	("C-x d"   . nil)
	("s-D"     . nil)
	("C-x t d" . nil)
	("C-x 4 d" . nil)
	("C-x 5 d" . nil))
  
  (:map dired-mode-map
	;; I think it makes more sense to delegate opening files to the RET key
	;; also using 'o' for preview (no switching) is nicer
	("S-<return>" . dired-find-file-other-window)
	("o"          . dired-display-file)
	("C-o"        . nil)

	;; disable mouse
	("<mouse-1>" . nil)
	("<mouse-2>" . nil)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minibuffer
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple
  :custom
  ;; shadow mode makes the dimmed path invisible
  (file-name-shadow-mode t))

(use-package embark
  :bind
  (:map global-map
	("C-." . embark-act)
	([remap describe-bindings] . embark-bindings)))

;;; History

(use-package savehist
  :custom
  (savehist-mode t)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Eldoc

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)) ;; lighter

;;; Flymake (diagnostics)

(use-package flymake
  :custom
  (flymake-mode-line-counter-format
   '("" flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ""))
  
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-no-changes-timeout 0.25)
  (flymake-mode-line-lighter nil))

(use-package consult
  :bind
  (:map prog-mode-map
	("C-c c d" . consult-flymake)))

;;; Apheleia (formatter)

(use-package prog-mode
  :bind
  (:map prog-mode-map
	("C-c c f" . apheleia-format-buffer)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lang: Emacs Lisp
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elisp-mode
  :hook (emacs-lisp-mode-hook . flymake-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lang: Nix
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nix-ts-mode
  :mode (rx ".nix" eos))

;;; LSP

(use-package eglot
  :hook (nix-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd"))))

;;; Formatter

(use-package apheleia
  :config
  (setf (alist-get 'nixfmt apheleia-formatters)
	'("nixfmt"))
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist)
	'(nixfmt)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lang: Python
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :custom
  (python-indent-guess-indent-offset nil))

(use-package treesit
  :after python
  ;; use treesitter over traditional major mode
  :hook (python-mode-hook . python-ts-mode))

;;; LSP

(use-package eglot
  :after python
  :hook (python-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff" "server"))))

;;; Formatter

(use-package apheleia
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lang: Gleam
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

;;; LSP

(use-package eglot
  :after gleam-ts-mode
  :hook (gleam-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(gleam-ts-mode . ("gleam" "lsp"))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assistant
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; gptel

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'deepseek-coder)
  (gptel-backend
   (gptel-make-openai "DeepSeek"
     :host "api.deepseek.com/v1"
     :endpoint "/chat/completions"
     :key (getenv "DEEPSEEK_KEY")
     :models '(deepseek-chat deepseek-coder)
     :stream t)))

(provide 'init)
;;; init.el ends here
