;;; annt-editing.el --- Editor Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 anntnzrb

;; Author: anntnzrb <anntnzrb@proton.me>
;; Keywords: package

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

(use-package evil
  :elpaca t
  :init
  ;; the following 2 need to be set before loading `evil'
  (setq evil-want-keybinding    nil)
  (setq evil-undo-system 'undo-redo) ;; EmacsV28+

  (evil-mode 1))

(provide 'annt-editing)
