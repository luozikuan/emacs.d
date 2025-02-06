;;; init-fonts.el --- Configure fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; font setup
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun set-font-for-script (font scripts)
  "Specify FONT for SCRIPTS, like emoji, han, cjk-misc"
  (dolist (charset scripts)
    (set-fontset-font t charset
                      (font-spec :family font) nil 'prepend)))

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
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
             return (set-font-for-script font '(symbol)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-font-for-script font '(emoji)))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei Mono" "PingFang SC" "Microsoft Yahei UI" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.2)))
                      (set-font-for-script font '(han cjk-misc))))))

(setup-fonts)
(add-hook 'window-setup-hook 'setup-fonts)
(add-hook 'server-after-make-frame-hook 'setup-fonts)


(provide 'init-fonts)
;;; init-fonts.el ends here
