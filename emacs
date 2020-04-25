; -*- mode: Lisp;-*-
;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

; disable some defaults
(setq backup-inhibited t)
(setq inhibit-startup-screen t)
(setq case-replace nil)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (list "%b@" (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; default to unified diffs
(setq diff-switches "-u")

;; Line/Column numbers
(global-linum-mode t)
(setq column-number-mode t)
;; One line scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step           1
     scroll-conservatively 10000)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Tabs and spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4)
(setq-default truncate-lines 0)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


;; C-mode setting
(defconst bsd-style-extend
  '("bsd"
    (c-basic-offset 4)
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "bsd-style-extend" bsd-style-extend)

(setq c-default-style "bsd-style-extend")

(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(show-paren-mode t)

;; Faces
(custom-set-faces
 '(ivy-current-match ((t (:background "blue violet" :foreground "dodger blue"))))
 '(region ((t (:background "cyan" :distant-foreground "gtk_selection_fg_color"))))
 '(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold))))

(set-face-attribute 'default nil :height 102)
(set-face-background 'fringe "LightGray")

(setq ivy-display-style 'fancy)

(if (window-system)
    (progn
      (normal-erase-is-backspace-mode t)
      (set-frame-height (selected-frame) 50)
      (set-frame-width  (selected-frame) 110))
)


(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path (file-truename "~/.emacs_modules/.."))

(require 'ext-modules)
(require 'own-mappings)
(require 'own-functions)
(require 'org-mode-settings)
