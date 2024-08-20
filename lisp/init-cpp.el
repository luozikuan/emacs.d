;;; init-cpp.el --- Settings for c++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; c++
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


(require 'c-ts-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; define ellemtel style
(defun my-c++-ts-mode-indent-style ()
  "Override the built-in BSD indentation style with some additional rules"
  `(
    ((node-is ")") parent-bol 0)
    ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)

    ((node-is "case_statement") parent-bol 0)
    ((parent-is "case_statement") parent-bol c-ts-mode-indent-offset)

    ;; Preproc
    ((node-is "preproc") column-0 0)
    ((node-is "#endif") column-0 0)

    ((node-is "compound_statement") standalone-parent 0)
    ((parent-is "for_statement") parent-bol c-ts-mode-indent-offset)
    ((parent-is "while_statement") parent-bol c-ts-mode-indent-offset)
    ((and (parent-is "if_statement")
          (not (node-is "else")))
     parent-bol c-ts-mode-indent-offset)
    ((and (parent-is "else")
          (not (node-is "compound_statement")))
     parent-bol c-ts-mode-indent-offset)
    ((parent-is "do_statement") parent-bol c-ts-mode-indent-offset)

    ;; Append here the indent style you want as base
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(defun my-c++-ts-mode-hook ()
  "Common setting for c++-ts-mode."
  (remove-hook 'flymake-mode-hook 'sanityinc/enable-flymake-flycheck)
  (display-fill-column-indicator-mode -1)
  (hs-minor-mode)
  ;; (origami-mode)
  ;; (which-function-mode)
  (local-set-key [mouse-8] 'xref-go-back)
  (local-set-key [mouse-9] 'xref-go-forward)
  (local-set-key (kbd "M-?") 'xref-find-references)
  (local-set-key (kbd "M-o") 'projectile-find-other-file)
  (local-set-key (kbd "C-c f") 'hs-toggle-hiding)
  (setq c-ts-mode-indent-offset 4
        tab-width 4
        indent-tabs-mode t
        c-ts-mode-indent-style #'my-c++-ts-mode-indent-style)
  )
(add-hook 'c++-ts-mode-hook #'my-c++-ts-mode-hook)


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
