;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "This config requires GNU Emacs %s or higher, but you're running %s" minver emacs-version)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; operating system detect
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/wslp
  (and sys/linuxp
       ;; (string-match-p "microsoft" (shell-command-to-string "uname -r"))
       (getenv "WSLENV"))
  "Are we running on a WSL system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")


;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))


;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; General performance tuning
(when (require-package 'gcmh)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-fonts)
(require 'init-ligature)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-general)
(require 'init-whitespace)
(require 'init-git)

(when sys/wslp
  (require 'init-wsl2))

(require 'init-projectile)

(require 'init-markdown)
(require 'init-json)
(require 'init-org)
(require 'init-prog-common)
(require 'init-cpp)
(require 'init-http)
(require 'init-python)
(require 'init-nix)

(require 'init-paredit)
(require 'init-lisp)

(require 'init-copilot)
(require 'init-gptel)
(require 'init-packages)
(require 'init-dashboard)

(require 'init-misc)
(require 'init-terminals)

;; Extra packages which don't require any configuration
(add-hook 'after-init-hook 'global-eldoc-mode)
(require 'init-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))



;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
