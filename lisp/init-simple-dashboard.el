;;; init-simple-dashboard.el --- simple dashboard config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'recentf)
(require 'magit)

;;; Code:

(defvar simple-dashboard-mode nil)
(defvar simple-dashboard-recentfiles '()
  "Recent list.")

;; (defvar recent-projects '()
;;   "List of recent projects.")

(defvar simple-dashboard-repositories '()
  "List of Magit repositories.")

(defcustom simple-dashboard-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'simple-dashboard
  :type '(natnum))

(defcustom simple-dashboard-path-max-length 72
  "Max length of the path."
  :group 'simple-dashboard
  :type '(natnum))

(defcustom simple-dashboard-image-file (locate-user-emacs-file "splash.svg")
  "Image file in simple-dashboard."
  :group 'simple-dashboard
  :type '(file))

(defcustom simple-dashboard-image-width 200
  "Image width for weather information."
  :group 'simple-dashboard
  :type '(natnum))

(defcustom simple-dashboard-image-height 140
  "Image width for weather information."
  :group 'simple-dashboard
  :type '(natnum))

(defgroup simple-dashboard nil
  "simple-dashboard group."
  :group 'applications)

(defconst simple-dashboard-buffer "*welcome*"
  "simple-dashboard buffer name.")

(defvar simple-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'simple-dashboard--open-recent-file)
    (define-key map (kbd "<return>") 'simple-dashboard--open-recent-file)
    (define-key map (kbd "o") 'simple-dashboard--open-recent-file)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "q") 'bury-buffer)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (number-to-string i))
                  `(lambda ()
                     (interactive)
                     (simple-dashboard--open-recent-file-at-index ,i))))

    (let ((repo-key ?a))
      (dolist (i (number-sequence 1 9))
        (define-key map (kbd (char-to-string (+ repo-key i -1)))
                    `(lambda ()
                       (interactive)
                       (simple-dashboard--open-repository-at-index ,i)))))
    map)

  "Keymap for `simple-dashboard-mode'.")

(define-derived-mode simple-dashboard-mode fundamental-mode "simple-dashboard"
  "Major mode for the simple-dashboard screen."
  :group 'simple-dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (use-local-map simple-dashboard-mode-map))

(defface simple-dashboard-title-face
  '((t :foreground "#87AAF8" :height 1.2))
  "Face added to code-usage display."
  :group 'simple-dashboard)

(defface simple-dashboard-info-face
  '((t :foreground "#F66D86" :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'simple-dashboard)

(defface simple-dashboard-text-info-face
  '((t :foreground "#ADB5D0" :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'simple-dashboard)

(defface simple-dashboard-path-face
  '((t :foreground "#63677D" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'simple-dashboard)

(defface simple-dashboard-filename-face
  '((t :weight semi-bold))
  "Face for the file name."
  :group 'simple-dashboard)

(defface simple-dashboard-time-face
  '((t :foreground "#a6adc8" :height 0.9))
  "Face for time."
  :group 'simple-dashboard)

(defface simple-dashboard-startup-time-face
  '((t :foreground "#F66D86" :height 0.9 :bold nil :italic nil))
  "Face for time."
  :group 'simple-dashboard)

(defface simple-dashboard-shortcut-face
  '((t :foreground "#b198ea" :height 0.9 :bold t))
  "Face for time."
  :group 'simple-dashboard)

(defun simple-dashboard--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun simple-dashboard--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (message file)
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun simple-dashboard--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files simple-dashboard-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun simple-dashboard--open-repository-at-index (index)
  "Open Magit status for the repository at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((repos simple-dashboard-repositories))
    (when (<= 1 index (length repos))
      (magit-status (nth (1- index) repos)))))

(defun simple-dashboard--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters and adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))

(defun simple-dashboard--insert-recent-files ()
  "Insert the first 9 recent files with icons in the simple-dashboard buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files simple-dashboard-recentfiles)
         (left-margin (simple-dashboard--calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s%s"
                            (propertize (simple-dashboard--truncate-path-in-middle file-dir simple-dashboard-path-max-length) 'face 'simple-dashboard-path-face)
                            (propertize file-name 'face 'simple-dashboard-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat (propertize (format "%s. " shortcut) 'face 'simple-dashboard-shortcut-face) title-with-path (propertize (format " [%s]" shortcut) 'face 'simple-dashboard-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun simple-dashboard--insert-repositories ()
  "Insert Magit repositories in the simple-dashboard buffer."
  (insert "\n")
  (let* ((repos simple-dashboard-repositories)
         (left-margin (simple-dashboard--calculate-padding-left)))
    (dolist (repo repos)
      (let* ((index (cl-position repo repos :test #'equal))
             (shortcut (char-to-string (+ ?a index)))
             (repo-path (file-name-directory repo))
             (repo-name (file-name-nondirectory repo))
             (title (format "%s%s"
                            (propertize (simple-dashboard--truncate-path-in-middle repo-path simple-dashboard-path-max-length) 'face 'simple-dashboard-path-face)
                            (propertize repo-name 'face 'simple-dashboard-filename-face)))
             (title-with-path (propertize title 'path repo-path))
             (title-with-path-and-shortcut (concat (propertize (format "%s. " shortcut) 'face 'simple-dashboard-shortcut-face) title-with-path (propertize (format " [%s]" shortcut) 'face 'simple-dashboard-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun simple-dashboard--calculate-padding-left ()
  "Calculate padding for left side."
  (if-let* ((files simple-dashboard-recentfiles)
            (max-length (apply 'max (mapcar (lambda (path) (length (simple-dashboard--truncate-path-in-middle path simple-dashboard-path-max-length))) files)))
            (filenames (mapcar (lambda (path) (file-name-nondirectory path)) files))
            (max-filename-length (/ (apply 'max (mapcar 'length filenames)) 2))
            (left-margin (max (+ simple-dashboard-min-left-padding max-filename-length) (/ (- (window-width) max-length) 2))))
      (- left-margin max-filename-length)
    simple-dashboard-min-left-padding))

(defun simple-dashboard--insert-text (text)
  "Insert (as TEXT)."
  (let ((left-margin (simple-dashboard--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun simple-dashboard--insert-magit ()
  "Insert the Magit repositories in the simple-dashboard buffer."
  (insert "\n\n")
  (simple-dashboard--insert-text "Magit Repositories:")
  (simple-dashboard--insert-repositories))

(defun simple-dashboard--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) simple-dashboard-buffer)
    (simple-dashboard--refresh-screen)))

;;;###autoload
(defun simple-dashboard-create-welcome-hook ()
  "Setup simple-dashboard screen."
  (when (< (length command-line-args) 2)
    (add-hook 'switch-to-buffer 'simple-dashboard--redisplay-buffer-on-resize)
    (add-hook 'window-size-change-functions 'simple-dashboard--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (simple-dashboard--refresh-screen)))))

(defun simple-dashboard--insert-startup-time ()
  "Insert startup time."
  (simple-dashboard--insert-text (format "%s %s %s"
                                          (propertize "Startup time:" 'face 'simple-dashboard-text-info-face)
                                          (propertize (emacs-init-time "%.2f") 'face 'simple-dashboard-startup-time-face)
                                          (propertize "seconds" 'face 'simple-dashboard-text-info-face))))


(defun simple-dashboard--insert-package-info (packages)
  "Insert package info as (PACKAGES)."
  (simple-dashboard--insert-text (format "%s %s"
                                          (propertize packages 'face 'simple-dashboard-info-face 'display '(raise -0.1))
                                          (propertize "packages loaded" 'face 'simple-dashboard-text-info-face 'display '(raise -0.1)))))

;; (defun insert-recent-projects ()
;;   "Insert recent projects."
;;   (projectile-mode +1)
;;   (setq recent-projects (projectile-relevant-known-projects))
;;   (dolist (project (seq-take recent-projects 3))
;;     (simple-dashboard--insert-text (projectile-project-name project))))

(defun simple-dashboard--package-length ()
  "Get the number of installed packages."
  (cond
   ((bound-and-true-p package-alist)
    (length package-activated-list))
   ((boundp 'straight--profile-cache)
    (hash-table-count straight--profile-cache))
   ((boundp 'elpaca--queued)
    (length elpaca--queued))
   (t 0)))

(defun simple-dashboard--get-magit-repos ()
  "Get the repos from magit"
  (mapcar (lambda (path)
            (abbreviate-file-name (directory-file-name (file-truename path))))
          (magit-list-repos)))

(defun simple-dashboard--refresh-screen ()
  "Show the simple-dashboard screen."
  (interactive)
  (setq simple-dashboard-recentfiles (seq-take recentf-list 9))
  (setq simple-dashboard-repositories (simple-dashboard--get-magit-repos))
  (with-current-buffer (get-buffer-create simple-dashboard-buffer)
    (let* ((buffer-read-only)
           (image (create-image simple-dashboard-image-file nil nil :width simple-dashboard-image-width :height simple-dashboard-image-height))
           (size (image-size image))
           (width (car size))
           (left-margin (max simple-dashboard-min-left-padding (floor (/ (- (window-width) width) 2))))
           (packages (format "%d" (simple-dashboard--package-length))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)

        (insert "\n\n")
        (simple-dashboard--insert-text (propertize "Recent Files" 'face 'simple-dashboard-title-face))
        (simple-dashboard--insert-recent-files)
        (setq cursor-type nil)
        (insert "\n\n")

        (simple-dashboard--insert-text (propertize "Magit Repositories" 'face 'simple-dashboard-title-face))
        (simple-dashboard--insert-repositories)
        (setq cursor-type nil)
        (insert "\n\n")

        (simple-dashboard--insert-startup-time)
        (simple-dashboard--insert-package-info packages)

        (insert "\n\n")
        (simple-dashboard--insert-centered (propertize (format-time-string "%A, %B %d %H:%M") 'face 'simple-dashboard-time-face))
        (switch-to-buffer simple-dashboard-buffer)
        (simple-dashboard-mode)
        (goto-char (point-min))

        (forward-line 5)))
    (setq buffer-read-only t)))

(simple-dashboard-create-welcome-hook)

(provide 'init-simple-dashboard)
;;; init-simple-dashboard.el ends here
