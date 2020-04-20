; -*- mode: Lisp;-*-
;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (list "%b@" (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; default to unified diffs
(setq diff-switches "-u")

;disable backup
(setq backup-inhibited t)

(setq case-replace nil)

(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

;; always end a file with a newline
;(setq require-final-newline 'query)

;; use del correctly
(normal-erase-is-backspace-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Tab width 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4)

(setq-default truncate-lines 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "blue violet" :foreground "dodger blue"))))
 '(region ((t (:background "cyan" :distant-foreground "gtk_selection_fg_color"))))
 '(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold))))

(setq ivy-display-style 'fancy)

(if (window-system)
  (progn
    (set-frame-height (selected-frame) 50)
    (set-frame-width  (selected-frame) 110))
)

;; One line scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step           1
     scroll-conservatively 10000)

(require 'ido)
(ido-mode t)

;;(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;;(add-to-list
;; 'auto-mode-alist
;; '("\\.m$" . matlab-mode))
;;(setq matlab-indent-function t)
;;(setq matlab-shell-command "matlab")
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))


(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(setq c-default-style "bsd"
  c-basic-offset 4)

(global-linum-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-face-attribute 'default nil :height 102)
(set-face-background 'fringe "LightGray")

(add-to-list 'load-path "~/.emacs_modules/ag.el")
(add-to-list 'load-path "~/.emacs_modules/dash.el")
(add-to-list 'load-path "~/.emacs_modules/editorconfig-emacs")
(add-to-list 'load-path "~/.emacs_modules/f.el")
(add-to-list 'load-path "~/.emacs_modules/flycheck")
(add-to-list 'load-path "~/.emacs_modules/hydra.git")
(add-to-list 'load-path "~/.emacs_modules/magit/lisp")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")
(add-to-list 'load-path "~/.emacs_modules/s.el")
(add-to-list 'load-path "~/.emacs_modules/seq")
(add-to-list 'load-path "~/.emacs_modules/shut-up")
(add-to-list 'load-path "~/.emacs_modules/swiper")
(add-to-list 'load-path "~/.emacs_modules/transient/lisp")
(add-to-list 'load-path "~/.emacs_modules/undercover.el")
(add-to-list 'load-path "~/.emacs_modules/with-editor")

;(byte-recompile-directory "~/.emacs_modules/dash.el" 0)
(byte-recompile-directory "~/.emacs_modules/editorconfig-emacs" 0)
(if (version< emacs-version "26")
    (message "Skipping bytecompile: f.el")
  ;(byte-recompile-directory "~/.emacs_modules/f.el" 0)
  )
(byte-recompile-directory "~/.emacs_modules/hydra.git" 0)
(if (version< emacs-version "25")
    (message "Skipping bytecompile...")
  (byte-recompile-directory "~/.emacs_modules/ag.el" 0)
  ;(byte-recompile-directory "~/.emacs_modules/magit/" 0)
  (byte-recompile-directory "~/.emacs_modules/s.el" 0)
  (byte-recompile-directory "~/.emacs_modules/seq" 0)
  (byte-recompile-directory "~/.emacs_modules/swiper" 0)
  (byte-recompile-directory "~/.emacs_modules/transient" 0)
  )

(byte-recompile-directory "~/.emacs_modules/with-editor" 0)

(require 'ag)
(require 'counsel)
(require 'editorconfig)
(require 'swiper)
(if (version< emacs-version "25")
    (message "Skipping magit")
  (require 'magit)
)
(require 'multiple-cursors)
(editorconfig-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck)

(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'load-path (file-truename "~/.emacs_modules/.."))
(require 'own-mappings)
(require 'own-functions)
