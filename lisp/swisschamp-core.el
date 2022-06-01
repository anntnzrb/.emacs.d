;;; swisschamp-core.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; This file contains core function & macro declarations only.

;; Helpers, sugars and wrappers should be placed at 'swisschamp-herlper.el'.

;;; Code:

;; The following `after!' macro allows deferring (lazy loading) packages, just
;; like the ':defer' keyword in `use-package'.
;; This was taken from Doom Emacs' core.
;; <https://github.com/doomemacs/doomemacs/blob/1b8f46c7c5893d63e4bcebc203c0d28df9f5981b/core/core-lib.el#L506-L549>
(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them.  These are package names, not modes,
functions or variables.  It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. Supports compound package statements (see below)
3. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            ;; We intentionally avoid `with-eval-after-load' to prevent eager
            ;; macro expansion from pulling (or failing to pull) in autoloaded
            ;; macros/packages.
            `(eval-after-load ',package ',(macroexp-progn body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro featurep! (feature &rest body)
  "Eval BODY if FEATURE is present."
  `(if (featurep ,feature) (progn ,@body)))

(provide 'swisschamp-core)
;;; swisschamp-core.el ends here
