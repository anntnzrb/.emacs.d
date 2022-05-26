;;; early-init.el --- GNU Emacs >= 27.1 pre-initialization file -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 anntnzrb

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

;; At this early stage there a few settings to be set in order to set-up Emacs
;; efficiently, this file should not be long at all.  All other usual
;; configuration should remain elsewhere.  This fill is only valid for GNU
;; Emacs 27.1+.  Gets loaded before the 'init.el' file.

;; This file is loaded before 'package.el' and the first graphical frame are
;; initialized.

;; You should find more information regarding this particular file at
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>.

;;; Code:

;; check if using minimum required version
(let ((min-ver "28.0"))
  (when (version< emacs-version min-ver)
    (error "Your version of GNU Emacs v%s is outdated, you need at least v%s"
           emacs-version min-ver)))

;; The garbage collector threshold is increased here to prevent it from
;; running at these early stages, the objective here is to reset it later.
;; Consider checking the 'init.el' file right next to this file for the
;; remaining instructions.
;; WARNING :: Improper adjustment of these settings properly may lead to
;; freezes/stuttering and unexpected behaviour.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Increase the amount of data which Emacs reads from processes
(setq read-process-output-max (* 1024 1024))

;; Native Compilation
;; > Emacs v28
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available.")
      (setq native-comp-async-report-warnings-errors 'silent)
      ;; Prevent unwanted runtime compilation
      (setq native-comp-deferred-compilation nil))
  (message "Native compilation is NOT available."))

;; package tweaks
(setq package-enable-at-startup nil) ;; needed by `straight.el'

;; GUI
(setq inhibit-splash-screen        t)
(setq inhibit-startup-screen       t)
(setq frame-inhibit-implied-resize t) ;; don't frame-resize this early
(eval '(setq inhibit-startup-echo-area-message t))
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; dialogs; preferably disabled for a totally keyboard-driven experience
(setq use-dialog-box  nil)
(setq use-file-dialog nil)

(provide 'early-init)
;;; early-init.el ends here
