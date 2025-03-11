;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :custom
  (custom-safe-themes t))

(use-package auto-dark
  :ensure t
  :after color-theme-sanityinc-tomorrow
  :custom
  (auto-dark-themes '((sanityinc-tomorrow-night) (sanityinc-tomorrow-day)))
  :init
  (auto-dark-mode)
  (diminish 'auto-dark-mode)
  ;; Toggle between light and dark
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (auto-dark--set-theme 'light))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (auto-dark--set-theme 'dark))
  )

(provide 'init-themes)
;;; init-themes.el ends here
