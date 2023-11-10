;;; init-fonts.el --- Configure fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    ;; "Cascadia Code" "Fira Code" "Jetbrains Mono" "SF Mono" "Hack" "Source Code Pro" "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas"
    (set-face-attribute 'default nil
                        :family "Cascadia Code"
                        :height (cond (sys/macp 140)
                                      (sys/win32p 95)
                                      ;; (sys/wslp 125)
                                      (t 100)))

    ;; Specify font for all unicode characters
    ;; "Segoe UI Symbol" "Symbola" "Symbol"
    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)

    ;; Emoji
    ;; "Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)

    ;; Specify font for Chinese characters
    ;; "WenQuanYi Micro Hei Mono" "PingFang SC" "Microsoft Yahei" "STFangsong"
    (let ((font "WenQuanYi Micro Hei Mono"))
      (setq face-font-rescale-alist `((,font . 1.2)))
      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font)))
    ))

(setup-fonts)
(add-hook 'window-setup-hook 'setup-fonts)
(add-hook 'server-after-make-frame-hook 'setup-fonts)


(provide 'init-fonts)
;;; init-fonts.el ends here
