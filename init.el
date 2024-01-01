;; GNU Emacs initialisation -*- lexical-binding: t; -*-

(defvar annt/load-dirs '(; "lib/annt"
			 "modules/annt")
  "List of directories containing modules and libraries to add to `load-path'")

(mapc
 (lambda (dir)
   (add-to-list 'load-path (locate-user-emacs-file dir)))
 annt/load-dirs)

(defvar annt/init--config-literate
  (expand-file-name "readme.org" user-emacs-directory)
  "File path of the literate configuration file.")

(defun annt/init--tangle-literate-config ()
  "Prompt to tangle literate configuration file."
  (interactive)
  (when (and (equal (buffer-file-name) annt/init--config-literate)
             (y-or-n-p "Save and tangle before exiting? "))
    (save-buffer)
    (org-babel-tangle)))

(add-hook 'kill-emacs-hook #'annt/init--tangle-literate-config)

(defmacro annt/helpers--add-hook (hooks functions &optional depth local)
  "Replacement and wrapper for `add-hook'.
Add N FUNCTIONS to M HOOKS. Both optional DEPTH and LOCAL arguments are passed
to `add-hook'.

NOTE: The mode hook should not be quoted."
  (let ((hooksp (listp hooks))
        (fnsp   (listp functions)))
    (cond
     ;; both HOOKS and FUNCTIONS are lists
     ((and hooksp fnsp)
      `(mapc (lambda (hk)
               (mapc (lambda (fn)
                       (add-hook 'hk fn ,depth ,local))
                     ,functions))
             ,hooks))

     ;; only HOOKS is a list
     (hooksp
      `(mapc (lambda (hk)
               (add-hook 'hk ,functions ,depth ,local))
             ,hooks))

     ;; only FUNCTIONS is a list
     (fnsp
      `(mapc (lambda (fn)
               (add-hook ',hooks fn ,depth ,local))
             ,functions))

     ;; neither HOOKS nor FUNCTIONS is a list
     ((not (or hooksp fnsp))
      `(add-hook ',hooks ,functions ,depth ,local))

     ;; fallback to error message
     (t `(error "Invalid arguments: hooks and functions should be either a symbol or a list")))))

(defalias 'add-hook! #'annt/helpers--add-hook)

;;;###autoload
(defun append-path (path)
  "Add the specified PATH to the `exec-path' and 'PATH' environment variable."
  (let ((expanded-path (expand-file-name path)))
    (if (file-directory-p expanded-path)
        (progn
          (add-to-list 'exec-path expanded-path)
          (setenv "PATH" (concat (getenv "PATH") path-separator expanded-path)))
      (warn "Warning: %s is not a directory" expanded-path))))

;;;###autoload
(defun termbin-upload (start end)
  "Uploads region between START and END to termbin.com and copies resulting URL.

This function operates asynchronously."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (proc (make-process :name "termbin-upload"
                             :command '("nc" "termbin.com" "9999")
                             :connection-type 'pipe
                             :buffer nil
                             :filter (lambda (_ string)
                                       (let ((trimmed-string (string-trim-right string "[\n\0]+")))
                                         (message "Uploaded and copied URL: %s" trimmed-string)
                                         (kill-new trimmed-string))))))
    (message "Uploading...")
    (process-send-string proc content)
    (process-send-eof proc)))

(use-package cus-edit
  :config
  (setopt custom-buffer-done-kill t))

(setopt custom-file (make-temp-file "emacs-custom-file-"))

(use-package package
  :config
  (setopt package-vc-register-as-project nil)
  (setopt package-archives
          '(("gnu-elpa"       . "https://elpa.gnu.org/packages/")
            ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
            ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
            ("melpa"          . "https://melpa.org/packages/")))

  (setopt package-archive-priorities
          '(("gnu-elpa" . 3)
            ("melpa"    . 2)
            ("nongnu"   . 1)))

  ;; disallow built-ins to be externally managed
  (setopt package-install-upgrade-built-in t))

(require 'annt-mod-simple)
(require 'annt-mod-editing)

(require 'annt-mod-vi)
(require 'annt-mod-org)
(require 'annt-mod-file)
(require 'annt-mod-dired)
(require 'annt-mod-assist)
(require 'annt-mod-modeline)
(require 'annt-mod-appearance)
(require 'annt-mod-completion)
(require 'annt-mod-minibuffer)
