;;; init.el --- Initialization file of GNU Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; This initialization file configures the literate configuration file written
;; in Org, it does so by checking when it was last updated and tangles it only
;; when needed, loads the non-updated file otherwise.  The main goal here is to
;; remove the unneeded tangling and improve startup time.

;; There are a few other options I like to keep here, should be well-documented.

;; Lastly, there a few benchmarks to debug the initialization.

;;; Code:

(defconst swisschamp--file "swisschamp"
  "Base name of Swisschamp's configuration file.")

;; Add extra paths, 'lisp/' contains core & helper definitions.
(dolist (path '("lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; WARNING: Reset garbage collector (should be at the end of this file)
;; After everything else is set-up, set the garbage collector to a considerable
;; non-archaic value.
(defun swc-init--setup-gc ()
  "Sets up efficient garbage collector settings.
The following values are modified: `gc-cons-threshold' and
`gc-cons-percentage'

NOTE: Garbage collector was also previously modified at 'early-init.el'."
  (setq gc-cons-threshold (* 20 1024 1024))
  (setq gc-cons-percentage 0.1))

(defun swc-init--debug-init()
  "Displays information related to initialization."
  (interactive)
  (let ((pkg-count 0)
        (init-time (emacs-init-time)))

    ;; package.el
    (when (bound-and-true-p package-alist)
      (setq pkg-count (length package-activated-list)))

    ;; straight.el
    (when (boundp 'straight--profile-cache)
      (setq pkg-count (+ (hash-table-count straight--profile-cache) pkg-count)))

    (swisschamp--notify-and-log
     (format
      "GNU Emacs initialized in %s (%d pkgs) :: performed %d garbage collections."
      init-time pkg-count gcs-done))))

(defun swisschamp--notify-and-log (message)
  "Prints MESSAGE and logs it to a file in `user-emacs-directory' directory."
  (message message)

  ;; log to file (suppress stdout)
  (let ((inhibit-message t)
        (message-log-max nil))
    (append-to-file
     (format "[%s] :: %s\n" (current-time-string) message)
     nil
     (expand-file-name "emacs.log" user-emacs-directory))))

(defun swc-init--expand-file-name (file extension)
  "Return canonical path to FILE to Emacs config with EXTENSION."
  (locate-user-emacs-file
   (concat file extension)))

(defun swc-init--org-tangle-and-byte-compile (file target-file)
  "Tangle given FILE to TARGET-FILE and byte-compile it."
  (require 'ob-tangle)
  (org-babel-tangle-file file target-file)
  (byte-compile-file          target-file))

(defun swc-init--update-files ()
  "If configuration files have been modified, update them.
The Org file is compared with the tangled '.el' file; if the latter is older
than the Org file, delete the '.el' file code and re-tangle it, byte-compile it
afterwards."
  (interactive)
  (let* ((file swisschamp--file)
         (file-org (swc-init--expand-file-name file ".org"))
         (file-el  (swc-init--expand-file-name file ".el"))
         (file-elc (swc-init--expand-file-name file ".elc")))

    (when (or (file-newer-than-file-p file-org file-el)
              (not (file-exists-p file-elc)))
      (swisschamp--notify-and-log "Deleting old configurations for update...")
      (ignore-errors
        (delete-file file-el  t)
        (delete-file file-elc t))
      (swc-init--org-tangle-and-byte-compile file-org file-el))))

(defun swc-init--load-file ()
  "Load the configuration file.
This step should be done after tangling & byte-compiling."
  (let* ((file swisschamp--file)
         (file-org (swc-init--expand-file-name file ".org"))
         (file-el  (swc-init--expand-file-name file ".el")))

    ;; only tangle if '.el' file does not exists
    (unless (file-exists-p file-el)
      (swisschamp--notify-and-log "Literate configuration has not been tangled yet...")
      (swisschamp--notify-and-log "Proceeding to tangle & byte-compile configuration...")
      (swc-init--org-tangle-and-byte-compile file-org file-el)
      (swisschamp--notify-and-log "Literate configuration was tangled & byte-compiled."))

    ;; finally load the configuration file
    (load file-el nil 'nomessage 'nosuffix)
    (swisschamp--notify-and-log "Swisschamp configuration loaded.")))

;;; Apply everything

;; a few settings...
(setq frame-title-format
      (format "%%b - GNU Emacs [%s] @ %s" window-system (system-name)))

;; set working directory to '~/' regardless of where Emacs was started from
(cd (expand-file-name "~/"))

(add-hook 'emacs-startup-hook #'swc-init--setup-gc)
(add-hook 'emacs-startup-hook #'swc-init--debug-init)

;; set to both hooks to ensure config is always updated and byte-compiled, for
;; startup times it's possible to only keep `kill-emacs-hook'.
(add-hook 'emacs-startup-hook #'swc-init--update-files)
(add-hook 'kill-emacs-hook    #'swc-init--update-files)

(swc-init--load-file)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("swc-" . "swisschamp-"))
;; End:
