;;; early-init.el --- GNU Emacs >= 29.4 pre-initialization file-*- lexical-binding: t; no-byte-compile: t; -*-

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

;; At this early stage there a few settings to be set in order to set-up Emacs
;; efficiently.  All other usual configuration should remain elsewhere.  This
;; file is only valid for GNU Emacs >= 27.1.

;;; Code:

;; check if using minimum required version
(let ((min-ver "29.0"))
  (when (version< emacs-version min-ver)
    (error "Your version of GNU Emacs v%s is outdated, you need at least v%s"
           emacs-version min-ver)))

;;; Optimization tweaks

;; Increase the amount of data which Emacs reads from processes
(setopt read-process-output-max (expt 2 24))

;; misc
(setopt auto-window-vscroll nil)
(setopt message-log-max (expt 2 14))

;; The garbage collector threshold is increased here to prevent it from running
;; at these early stages, the objective here is to "disable" it temporarily and
;; later on re-configure it.
;; In easy words: `gc-cons-threshold' is set to `most-positive-fixnum' so the
;; this amount is "never" reached, therefore there is no garbage collection.
;; NOTE: Improper adjustment of these settings may lead to freezes/stuttering
;; and unexpected behaviour.
(setopt gc-cons-threshold most-positive-fixnum)
(setopt gc-cons-percentage 0.6)

;; `file-name-handler-alist' is consulted on each `require' and `load', it is
;; possible to `nil' for performance gains; should be reset after initialization.
(defconst FILE-NAME-HANDLER-ALIST-BAK file-name-handler-alist)
(setopt file-name-handler-alist nil)

;; finally, restore or set appropiate values for the modified symbols and
;; perform a garbage collection at the end once the configuration is done
;; setting up.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt file-name-handler-alist FILE-NAME-HANDLER-ALIST-BAK)
            (setopt gc-cons-threshold (* 20 1024 1024))
            (setopt gc-cons-percentage 0.1)
            (makunbound 'FILE-NAME-HANDLER-ALIST-BAK)
            (garbage-collect)) t)

;;; Optimization tweaks end here

;;; Native Compilation

;; emacs28+
(use-package emacs
  :if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  :custom
  (native-comp-async-report-warnings-errors 'silent)
  ;; Prevent compilation at runtime
  (native-comp-deferred-compilation nil))

;;; Native Compilation ends here

;; explicitely set Emacs' directory for this profile
(setopt user-emacs-directory
      (file-name-as-directory
       (concat (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config"))
               "/emacs")))

;;; GUI
(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (inhibit-startup-screen t)
  (frame-inhibit-implied-resize t) ;; don't frame-resize this early

  ;; dialogs preferably disabled for a totally keyboard-driven experience
  (use-dialog-box  nil)
  (use-file-dialog nil)

  ;; disable redisplay as it is not really needed, enable afterwards.
  (inhibit-redisplay t)

  :config
  (menu-bar-mode   -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1)

  ;; supress echo area startup message
  (fset 'display-startup-echo-area-message (lambda ()))
  :hook
  (window-setup-hook . (lambda ()
			 (setopt inhibit-redisplay nil)
			 (redisplay))))

;;; GUI ends here

(provide 'early-init)
;;; early-init.el ends here
