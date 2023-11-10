;;; init-cpp.el --- Settings for c++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-add-style "zac-style"
             '("ellemtel"
               (c-basic-offset . 4)
               (tab-width . 4)
               (indent-tabs-mode . t)
               (c-offsets-alist
                (case-label . 0)
                (arglist-cont-nonempty . +))))

(defun my-c++-mode-hook ()
    "Common setting for c++ mode."
    (c-set-style "zac-style")
    (display-fill-column-indicator-mode -1)
    (hs-minor-mode)
    (local-set-key [mouse-8] 'xref-go-back)
    (local-set-key [mouse-9] 'xref-go-forward)
    (local-set-key (kbd "M-o") 'projectile-find-other-file)
    (local-set-key (kbd "C-c f") 'hs-toggle-hiding))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;; cmake
(when (maybe-require-package 'cmake-mode)
  (with-eval-after-load 'projectile
    (setq projectile--cmake-manual-command-alist
          '((:configure-command . "cmake -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE -DCMAKE_BUILD_TYPE:STRING=Debug -S . -B build")
            (:compile-command . "cmake --build build --config Debug --target all")
            (:test-command . "cmake --build build --target test")
            (:install-command . "cmake --build build --target install")))))


(provide 'init-cpp)
;;; init-cpp.el ends here
