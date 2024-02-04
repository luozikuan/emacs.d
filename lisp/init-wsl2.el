;;; init-wsl2.el --- wsl2 relative settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; teach Emacs how to open links with your default browser
(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program  cmd-exe
          browse-url-generic-args     cmd-args
          browse-url-browser-function 'browse-url-generic
          search-web-default-browser 'browse-url-generic)))

(when (and (executable-find "fcitx5")
           (file-exists-p (locate-user-emacs-file "rime"))
           (display-graphic-p))
  ;; run this command first:
  ;;     ln -sf ~/.local/share/fcitx5/rime ~/.emacs.d/rime
  ;; since emacs is the only app need ime, so its ok
  (when (maybe-require-package 'rime)
    (setq default-input-method "rime")))

(when (featurep 'pgtk)
  ;; copy/paste using C-<insert>/S-<insert>
  (defun wsl-copy (start end)
    "Copy region to Windows clipboard."
    (interactive "r")
    (let ((coding-system-for-write 'gbk))
      (call-process-region start end "clip.exe" nil 0))
    (deactivate-mark))
  (defun wsl-clipboard-to-string ()
    "Return Windows clipboard as string."
    (let ((coding-system-for-read 'gbk))
      (string-trim-right
       ;; FIXME: sometimes this cmd do not return, lead to ui fronzen
       (shell-command-to-string "powershell.exe -Command Get-Clipboard")
       "\n")))
  (defun wsl-paste (arg)
    "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
    (interactive "P")
    (let ((clip (wsl-clipboard-to-string)))
      (insert clip)
      (if arg (kill-new clip))))
  (global-set-key (kbd "C-<insert>") #'wsl-copy)
  (global-set-key (kbd "S-<insert>") #'wsl-paste)
  )


(provide 'init-wsl2)
;;; init-wsl2.el ends here
