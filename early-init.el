;;; early-init.el --- GNU Emacs >= 27.1 pre-initialization file-*- lexical-binding: t; no-byte-compile: t; -*-

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

(setq package-enable-at-startup nil)
