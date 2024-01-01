(use-package ef-themes
  :ensure t
  :demand t
  :config
  (ef-themes-load-random 'dark)

  :bind
  (:map annt-prefix-map
	("a t e SPC" . ef-themes-select)
	("a t e e"   . ef-themes-load-random)))

(use-package fontaine
  :ensure t
  :demand t
  :init
  (setq-default text-scale-remap-header-line t)

  (setopt fontaine-presets
          '((t
             :bold-family nil
             :bold-weight bold
             :italic-family nil
             :italic-slant italic

             :default-family "Iosevka Comfy"
             :default-weight regular
             :default-height 120

             :fixed-pitch-family nil
             :fixed-pitch-weight nil
             :fixed-pitch-height 1.0

             :fixed-pitch-serif-family nil
             :fixed-pitch-serif-weight nil
             :fixed-pitch-serif-height 1.0

             :variable-pitch-family nil
             :variable-pitch-weight nil
             :variable-pitch-height 2.0

             :line-spacing nil)
	    (big
             :default-family "Jetbrains Mono Nerd Font"
             :default-height 180)))

  (fontaine-set-preset 't)

  :bind
  (:map annt-prefix-map
	("a f f" . fontaine-set-face-font)
	("a f SPC" . fontaine-set-preset)))

(use-package nerd-icons
  :defer 3
  :ensure t)

(provide 'annt-mod-appearance)
