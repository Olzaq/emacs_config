;;; package --- Summary

;;; Commentary:

;;; Code:

; Make sure submodules are initialized on the 1st run
(unless (file-readable-p (file-truename "~/.emacs_modules/use-package/use-package.el"))
  (error "Submodules are probably not installed.  Run git submodule update --init'"))

(add-to-list 'load-path "~/.emacs_modules/use-package")
(require 'use-package)

; Make sure org is properly installed
(add-to-list 'load-path "~/.emacs_modules/org-mode/lisp")
(unless (file-readable-p (file-truename "~/.emacs_modules/org-mode/lisp/org-loaddefs.el"))
  (error "Org-mode not properly installed.  Run 'make autoloads'"))
(require 'org-loaddefs)
(require 'org)

(straight-use-package 'ag)

(use-package dash
  :straight t)

(straight-use-package 'dockerfile-mode)
(straight-use-package 'editorconfig)
(straight-use-package 'expand-region)
(use-package f
  :straight t)

(unless running-in-termux
  (straight-use-package 'flycheck))

(use-package ht
  :straight t)

(straight-use-package 'levenshtein)

(use-package markdown-mode
  :straight t)

(straight-use-package 'multiple-cursors)

(use-package s
  :straight t)

(use-package seq
  :straight t)

(use-package spinner
  :straight t)

(straight-use-package 'swiper)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(straight-use-package 'zygospore)

(add-to-list 'load-path "~/.emacs_modules/company-irony")
(add-to-list 'load-path "~/.emacs_modules/company-mode")
(add-to-list 'load-path "~/.emacs_modules/csv-mode")
(add-to-list 'load-path "~/.emacs_modules/emacs-async")
(add-to-list 'load-path "~/.emacs_modules/hydra.git")
(add-to-list 'load-path "~/.emacs_modules/irony-mode")
(add-to-list 'load-path "~/.emacs_modules/move-text")
(add-to-list 'load-path "~/.emacs_modules/shut-up")
(add-to-list 'load-path "~/.emacs_modules/undercover.el")

(unless running-in-termux
  (require 'company)
  (require 'counsel)

  (require 'find-lisp)
  (require 'move-text)

  (unless use-lsp-mode
    (straight-use-package 'cmake-ide)
    (straight-use-package 'rtags)
    (require 'irony)
    (require 'irony-cdb)

    (set-variable 'rtags-path (file-truename "~/.emacs_modules/rtags/bin"))
    (cmake-ide-setup)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))

  (editorconfig-mode 1)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  (move-text-default-bindings)

  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t)
)

(require 'ido)
;;(if running-in-termux (ido-mode t))

(if (version< emacs-version "25")
      (message "Skipping some modules...")
  (unless running-in-termux
    (add-hook 'after-init-hook #'global-flycheck-mode))
    (require 'git-settings))


(provide 'ext-modules)
;;; ext-modules.el ends here
