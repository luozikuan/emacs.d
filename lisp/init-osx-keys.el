;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when sys/macp
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  )

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
