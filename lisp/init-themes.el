;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-tomorrow)

(setq custom-safe-themes t)

(defvar light-theme 'sanityinc-tomorrow-day
  "Default dark theme.")

(defvar dark-theme 'sanityinc-tomorrow-night
  "Default light theme.")

(when sys/linuxp
  (defun theme-from-dbus (value)
    "Change the theme based on a D-Bus property.

VALUE should be an integer or an arbitrarily nested list that
contains an integer.  When VALUE is equal to 2 then a light theme
will be selected, otherwise a dark theme will be selected."
    (load-theme (if (= 2 (car (flatten-list value)))
                    light-theme
                  dark-theme)
                t))

  (require 'dbus)
  ;; Set the current theme based on what the system theme is right now:
  (dbus-call-method-asynchronously
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   #'theme-from-dbus
   "org.freedesktop.appearance"
   "color-scheme")

  ;; Register to be notified when the system theme changes:
  (dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   (lambda (path var value)
     (when (and (string-equal path "org.freedesktop.appearance")
                (string-equal var "color-scheme"))
       (theme-from-dbus value))))
  )

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
  (set-default-theme 'light)
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (set-default-theme 'dark)
  (reapply-themes))


(provide 'init-themes)
;;; init-themes.el ends here
