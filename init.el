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

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'annt-core)
(require 'annt-buffer)
(require 'annt-minibuffer)
(require 'annt-completion)
(require 'annt-files)
(require 'annt-ui)
(require 'annt-dired)
(require 'annt-project)
(require 'annt-prog)
(require 'annt-edit)
(require 'annt-direnv)
(require 'annt-lsp)

(require 'annt-lang-nix)
(require 'annt-lang-elisp)
(require 'annt-lang-gleam)

(provide 'init)
;;; init.el ends here
