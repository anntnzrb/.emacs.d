(let ((emacs-version-required "29.2"))
  (cond ((version< emacs-version emacs-version-required)
         (error "Emacs %s or newer is required; you're running %s" emacs-version-required emacs-version))
        ((not (version= emacs-version emacs-version-required))
         (warn "This configuration is tested with Emacs %s; you're running %s" emacs-version-required emacs-version))))

(defconst annt/init--file-name-handler-alist file-name-handler-alist)
(defconst annt/init--vc-handled-backends     vc-handled-backends)

(setopt gc-cons-threshold  most-positive-fixnum)
(setopt gc-cons-percentage 0.5)

(setopt file-name-handler-alist nil)
(setopt vc-handled-backends     nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (setopt gc-cons-threshold (* 1024 1024 20))
              (setopt gc-cons-percentage 0.2)
              (setopt file-name-handler-alist annt/init--file-name-handler-alist)
              (setopt vc-handled-backends annt/init--vc-handled-backends))))

(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent)
  (setopt native-compile-prune-cache t))

(setopt inhibit-x-resources         t)
(setopt inhibit-splash-screen       t)
(setopt inhibit-startup-screen      t)
(setopt inhibit-startup-buffer-menu t)
(setopt inhibit-startup-echo-area-message user-login-name)

(setopt frame-resize-pixelwise       t)
(setopt frame-inhibit-implied-resize t)

(setopt frame-title-format '("%b @ emacs"))

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

(eval-after-load "startup"
  '(fset 'display-startup-echo-area-message (lambda ())))

(setopt package-enable-at-startup nil)
