;;; init-dashboard.el --- dashboard config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'dashboard)

(defun expand-banner-path (file)
  (concat user-emacs-directory
          (file-name-as-directory "banner")
          file))

(setq dashboard-startup-banner (cons (expand-banner-path "emacs.svg")
                                     (expand-banner-path "emacs.txt"))
      dashboard-image-banner-max-width 200
      dashboard-image-banner-max-height 140
      dashboard-center-content t
      ;; dashboard-show-shortcuts nil
      dashboard-items '((recents  . 10)
                        (projects . 10)
                        (bookmarks . 5)
                        )
      dashboard-projects-backend 'projectile
      dashboard-item-names '(("Projects:" . "Magit Projects:"))

      ;; Format: "(icon title help action face prefix suffix)"
      dashboard-navigator-buttons
      `(;; line1
        ((""
          "Packages"
          "List packages"
          (lambda (&rest _) (list-packages)))
         )
        ;; line 2
        )

      dashboard-startupify-list '(;; dashboard-insert-banner
                                  dashboard-insert-newline
                                  ;; dashboard-insert-banner-title
                                  ;; dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  )
      )

;; use magit to open project
(require-package 'magit)
(setq dashboard-projects-switch-function 'magit-status)

(dashboard-setup-startup-hook)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
