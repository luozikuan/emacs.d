;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless sys/wslp
  (use-package auto-dark
  :ensure t
  :custom
  (custom-safe-themes t)
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
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
  ))

(provide 'init-themes)
;;; init-themes.el ends here
