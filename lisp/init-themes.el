;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-tomorrow)

(setq custom-safe-themes t)

(defun set-default-theme (appearance)
  "Load theme based on current system APPEARANCE."
  (setq-default custom-enabled-themes
                (pcase appearance
                  ('light '(sanityinc-tomorrow-day))
                  ('dark '(sanityinc-tomorrow-night)))))


(defun determine-appearance ()
  "Determine system appearance for theme."
  (cond
   (sys/win32p
    (let* ((light-mode (string-trim (shell-command-to-string
                                      "powershell -Command \"(Get-ItemProperty -Path HKCU:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize -Name AppsUseLightTheme -ErrorAction SilentlyContinue).AppsUseLightTheme\"")))
           (appearance (if (equal light-mode "1")
                           'light
                         'dark)))
      appearance))
   ((display-graphic-p) 'light)
   (t 'dark)))

(set-default-theme (determine-appearance))


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(provide 'init-themes)
;;; init-themes.el ends here
