;;; swisschamp-helper.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2020-2022 anntnzrb

;; Author: anntnzrb <anntnzrb@proton.me>

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

;; This file contains helpers, sugars and wrappers for the sake of verbose
;; syntax reduction and readability.

;;; Code:

(defun swisschamp--add-hook (hooks functions &optional depth local)
  "Replacement and wrapper for `add-hook'.
Add N FUNCTIONS to M HOOKS.  Both optional DEPTH and LOCAL arguments are passed
to `add-hook'."
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (hook hooks)
    (dolist (fn functions)
      (add-hook hook fn depth local))))

(defalias 'add-hook!  #'swisschamp--add-hook)

(defmacro swisschamp--defun-hook (name arglist hooks docstring &rest body)
  "Define NAME as a function and set its mandatory documentation as DOCSTRING.
ARGLIST and BODY behave the same as `defun'.  HOOKS is an unquoted list of
hooks to which the function will be applied to.

Optional DEPTH and LOCAL arguments from `add-hook' are currently unsupported.

This is useful for `buffer-local' variables, which need a function (or a
 lambda) to later be called via a hook."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (unless (stringp docstring)
    (error "swisschamp: Provided docstring is not an actual string"))
  `(progn
     (defun ,name ,arglist
       ,(format
         "%s\n\nThis function was automatically generated by `swisschamp--defun-hook'."
         docstring)
       ,@body)
     (dolist (hook (quote ,hooks))
       (add-hook hook (quote ,name)))))

(defalias 'defun-hook! #'swisschamp--defun-hook)

(provide 'swisschamp-helper)
;;; swisschamp-helper.el ends here
