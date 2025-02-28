;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless sys/win32p
  (set-selection-coding-system 'utf-8))

;; make time string shown as English, e.g. org-time-stamp
(setq system-time-locale "C")

(provide 'init-locales)
;;; init-locales.el ends here
