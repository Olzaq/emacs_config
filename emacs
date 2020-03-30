; -*- mode: Lisp;-*-
;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;; turn on font-lock mode

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; always end a file with a newline
;(setq require-final-newline 'query)

;; use del correctly
(normal-erase-is-backspace-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

(global-set-key (kbd "C-c a") (lambda () (interactive) (counsel-ag nil nil "-U --depth 30")))
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-x g") 'ag-project)
(setq ivy-display-style 'fancy)

;; Tab width 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4)

(setq-default truncate-lines 0)

(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)
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

(defun to-underscore () (interactive)
    (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil
    (region-beginning) (region-end)) (downcase-region (region-beginning) (region-end)))
)

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (camelscore txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))

(defun camelscore (s)
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	(dasherize  s))
	    ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	(camelcase  s))
	    ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	(colonize   s))
	    (t						(underscore s)) ))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(global-set-key (kbd "C-c c") 'camelscore-word-at-point)

(setq c-default-style "bsd"
  c-basic-offset 4)

(global-linum-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-face-attribute 'default nil :height 102)
(set-face-background 'fringe "LightGray")

(add-to-list 'load-path "~/.emacs_modules/dash.el")
(add-to-list 'load-path "~/.emacs_modules/s.el")
(add-to-list 'load-path "~/.emacs_modules/ag.el")
(add-to-list 'load-path "~/.emacs_modules/swiper")
(add-to-list 'load-path "~/.emacs_modules/editorconfig-emacs")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")

;(byte-recompile-directory "~/.emacs_modules/dash.el" 0)
(byte-recompile-directory "~/.emacs_modules/s.el" 0)
;(byte-recompile-directory "~/.emacs_modules/ag.el" 0)
;(byte-recompile-directory "~/.emacs_modules/swiper" 0)
(byte-recompile-directory "~/.emacs_modules/editorconfig-emacs" 0)

(require 'ag)
(require 'swiper)
(require 'counsel)
(require 'editorconfig)
(require 'multiple-cursors)
(editorconfig-mode 1)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c A") 'mc/mark-all-like-this)

(add-to-list 'load-path "~/.emacs.d/lisp")
