;;; init-fonts.el --- Configure fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; font setup
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("CaskaydiaCove Nerd Font" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 140)
                                                      (sys/win32p 95)
                                                      ;; (sys/wslp 125)
                                                      (t 100))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei Mono" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(setup-fonts)
(add-hook 'window-setup-hook 'setup-fonts)
(add-hook 'server-after-make-frame-hook 'setup-fonts)

;; use valign to provide visual alignment for Org Mode table on windows
(when (and sys/win32p
           (maybe-require-package 'valign))
  (require 'derived)
  (dolist (mode '(org-mode markdown-mode gfm-mode))
    (add-hook (derived-mode-hook-name mode) 'valign-mode)))

(provide 'init-fonts)
;;; init-fonts.el ends here
