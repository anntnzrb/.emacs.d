;;; annt-ui.el --- UI Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 anntnzrb

;; Author: anntnzrb <anntnzrb@proton.me>
;; Keywords: ui, appearance

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

;;; Code:

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package faces
  :config
  (let ((font-size 12))
    ;; default
    (set-face-attribute 'default nil
                        :font (format "Iosevka-%s" font-size))
    ;; code
    (set-face-attribute 'fixed-pitch nil
                        :font (format "FantasqueSansMono-%s" font-size))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; these themes also work well in Emacs terminal mode
(use-package modus-themes
  :init
  (load-theme 'modus-vivendi t t) ;; load modus-themes; don't enable yet
  :config
  (setq modus-themes-links         '(background italic))
  (setq modus-themes-region        '(no-extend bg-only accented))
  (setq modus-themes-syntax        '(green-strings yellow-comments))
  (setq modus-themes-mode-line     '(accented borderless))
  (setq modus-themes-org-blocks    'greyscale)
  (setq modus-themes-paren-match   '(bold underline))
  (setq modus-themes-lang-checkers 'straight-underline)
  (setq modus-themes-bold-constructs   t)
  (setq modus-themes-italic-constructs t)

  ;; if running via GUI, prefer light or dark theme depending on daytime
  ;; else (terminal) prefer dark mode
  (let ((time (string-to-number (format-time-string "%H"))))
    (if (and (display-graphic-p) (and (> time 9) (< time 18)))
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clock
(use-package time
  :init
  (add-hook 'emacs-startup-hook #'display-time-mode)
  :config
  (setq display-time-format      "%a @ %H:%M")
  (setq display-time-interval              50) ;; update faster to catch up
  (setq display-time-default-load-average nil))

;; indicators
(use-package emacs
  :config
  (setq column-number-mode   t)
  (setq size-indication-mode t)
  (setq column-number-indicator-zero-based nil))

(provide 'annt-ui)
