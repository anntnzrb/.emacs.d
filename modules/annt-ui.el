;;; annt-ui.el --- User Interface -*- lexical-binding: t; -*-

;;; Commentary:

;; User-Interface related configurations.

;;; Code:

;;; Frame
;; title bar, scroll

(use-package frame
  :custom
  (frame-title-format
   (format "%%b - GNU Emacs @ %s" (system-name)))
  :config
  ;; start system in fullscreen
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
  
  (use-package pixel-scroll
    :hook (emacs-startup . pixel-scroll-precision-mode)))

;;; Font

(use-package fontaine
  :demand t
  :if (display-graphic-p)
  :custom
  (x-underline-at-descent-line nil)
  (text-scale-remap-header-line t) ;; emacs28+

  (fontaine-presets
   '(;
     (zed-mononoki
      :default-family "ZedMono Nerd Font Mono"
      :variable-pitch-family "Mononoki Nerd Font"
      :variable-pitch-weight semilight)

     (fira-zed
      :default-family "FiraCode Nerd Font"
      :variable-pitch-family "ZedMono Nerd Font"
      :variable-pitch-weight semilight)

     ;; fallback
     (default)
     (t
      :default-family nil
      :default-weight regular
      :default-height 150

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

  :hook
  (emacs-startup . fontaine-mode)
  (emacs-startup . (lambda ()
                     ;; set last preset or fall back to desired style from `fontaine-presets'.
                     (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default)))))

;;; Theme

(use-package ef-themes
  :demand t
  :hook (emacs-startup . (lambda () (ef-themes-load-random 'light)))
  :bind
  (:map global-map
	("C-c t r" . ef-themes-rotate)
	("C-c t t" . ef-themes-select)))

;;; which-key

(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-lighter "")
  (which-key-separator " ➡ ")
  (which-key-prefix-prefix "... ")

  (which-key-add-column-padding 16)
  (which-key-max-display-columns 4)
  (which-key-max-description-length 40)

  ;; timings
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25))

(provide 'annt-ui)
;;; annt-ui.el ends here
