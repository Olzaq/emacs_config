;;; package --- Summary

;;; Commentary:

;;; Code:

; Make sure org is properly installed
(add-to-list 'load-path "~/.emacs_modules/org-mode/lisp")
(unless (file-readable-p (file-truename "~/.emacs_modules/org-mode/lisp/org-loaddefs.el"))
  (error "Org-mode not properly installed.  Run 'make autoloads'"))
(require 'org-loaddefs)
(require 'org)

(add-to-list 'load-path "~/.emacs_modules/ag.el")
(add-to-list 'load-path "~/.emacs_modules/company-irony")
(add-to-list 'load-path "~/.emacs_modules/company-mode")
(add-to-list 'load-path "~/.emacs_modules/dash.el")
(add-to-list 'load-path "~/.emacs_modules/dockerfile-mode")
(add-to-list 'load-path "~/.emacs_modules/editorconfig-emacs")
(add-to-list 'load-path "~/.emacs_modules/emacs-async")
(add-to-list 'load-path "~/.emacs_modules/expand-region")
(add-to-list 'load-path "~/.emacs_modules/f.el")
(add-to-list 'load-path "~/.emacs_modules/flycheck")
(add-to-list 'load-path "~/.emacs_modules/helm")
(add-to-list 'load-path "~/.emacs_modules/helm-gtags")
(add-to-list 'load-path "~/.emacs_modules/hydra.git")
(add-to-list 'load-path "~/.emacs_modules/irony-mode")
(add-to-list 'load-path "~/.emacs_modules/magit/lisp")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")
(add-to-list 'load-path "~/.emacs_modules/s.el")
(add-to-list 'load-path "~/.emacs_modules/seq")
(add-to-list 'load-path "~/.emacs_modules/shut-up")
(add-to-list 'load-path "~/.emacs_modules/swiper")
(add-to-list 'load-path "~/.emacs_modules/transient/lisp")
(add-to-list 'load-path "~/.emacs_modules/undercover.el")
(add-to-list 'load-path "~/.emacs_modules/with-editor")
(add-to-list 'load-path "~/.emacs_modules/zygospore.el")

;(byte-recompile-directory "~/.emacs_modules/dash.el" 0)
(byte-recompile-directory "~/.emacs_modules/editorconfig-emacs" 0)
(if (version< emacs-version "26")
    (message "Skipping bytecompile: f.el")
  ;(byte-recompile-directory "~/.emacs_modules/f.el" 0)
  )
(byte-recompile-directory "~/.emacs_modules/helm" 0)
(byte-recompile-directory "~/.emacs_modules/helm-gtags" 0)
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

(byte-recompile-directory "~/.emacs_modules/company-mode" 0)
(byte-recompile-directory "~/.emacs_modules/company-irony" 0)
(byte-recompile-directory "~/.emacs_modules/expand-region" 0)
(byte-recompile-directory "~/.emacs_modules/irony-mode" 0)
(byte-recompile-directory "~/.emacs_modules/org-mode/lisp" 0)
(byte-recompile-directory "~/.emacs_modules/with-editor" 0)
(byte-recompile-directory "~/.emacs_modules/zygospore.el" 0)

(require 'ag)
(require 'company)
(require 'counsel)
(require 'dockerfile-mode)
(require 'editorconfig)
(require 'expand-region)
(require 'find-lisp)
(require 'helm-config)
(require 'helm-gtags)
(require 'helm-grep)
(require 'ido)
(require 'irony)
(require 'irony-cdb)
(require 'multiple-cursors)
(require 'server)
(require 'swiper)
(require 'zygospore)

(if (version< emacs-version "25")
    (message "Skipping some modules...")
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (require 'flycheck)
  (require 'magit)
)

(editorconfig-mode 1)
(ido-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(helm-mode 1)
;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; key bindings
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))


(unless (server-running-p) (server-start))

(provide 'ext-modules)
;;; ext-modules.el ends here
