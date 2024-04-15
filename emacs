; -*- mode: Lisp;-*-
;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

(when (< emacs-major-version 25)
  (error "Too old emacs version!"))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         nil 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file ))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(when (< emacs-major-version 26)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  )

(when (< emacs-major-version 27)
  (package-initialize))

;; detect if running in termux
(defconst termux-data-path "/data/data/com.termux")
(defvar running-in-termux (file-directory-p termux-data-path))

;; detect if running in docker
(defun check-if-running-in-docker ()
  (let ((orig-buffer (current-buffer))
        (is-running-in-docker nil))
    (switch-to-buffer (make-temp-name "docker-check-output"))
    (shell-command "cat /proc/1/cgroup" t)
    (setq is-running-in-docker (search-forward "docker" nil t))
    (kill-buffer)
    (switch-to-buffer orig-buffer)
    is-running-in-docker))

(defvar running-in-docker (check-if-running-in-docker))

(if running-in-docker
    (unless (= (shell-command "ssh-add -l") 0)
      (error "Start ssh-agent first")))

;; disable some defaults
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
(if (< emacs-major-version 26)
    (global-linum-mode t)
    (global-display-line-numbers-mode 1))
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
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


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
(setq ivy-on-del-error-function #'ignore)
(setq ivy-use-virtual-buffers t)

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

(defconst use-lsp-mode t)

(unless running-in-termux
  (require 'server)
  (unless (server-running-p) (server-start)))

(require 'ext-modules)
(require 'helm-settings)

(unless running-in-termux
  (require 'git-settings)
  (require 'c-and-cpp-mode)
  (require 'gdb-settings))

(require 'own-mappings)
(require 'own-functions)
(require 'org-mode-settings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((flycheck-gcc-language-standard . "c++17")
     (flycheck-clang-language-standard . "c++17"))))
