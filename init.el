;;; init.el --- Initialization file of GNU Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020-2023 anntnzrb

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

;;; Code:

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(defcustom annt--use-custom-file-p nil
  "Enable/disable custom file."
  :type 'boolean
  :group 'custom-file)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/"))))
(setq make-backup-files     t)
(setq backup-by-copying     t)
(setq version-control       t)
(setq delete-old-versions   t)
(setq kept-new-versions     8)
(setq kept-old-versions     2)
(setq create-lockfiles    nil)

(setq custom-file
      (if annt--use-custom-file-p
          (expand-file-name "custom.el" user-emacs-directory)
        null-device))

(require 'annt-package)
(require 'annt-ui)
(require 'annt-editing)
(require 'annt-lsp)

(provide 'init)
