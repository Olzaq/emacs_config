;;; package --- Summary

;;; Commentary:

;;; Code:

(straight-use-package 'ag)

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config (autoload 'csv-mode "csv-mode"
            "Major mode for editing comma-separated value files." t))

(use-package dash
  :straight t)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(straight-use-package 'emacs-async)
(straight-use-package 'expand-region)
(use-package f
  :straight t)

(unless running-in-termux
  (straight-use-package 'flycheck))

(use-package ht
  :straight t)

(straight-use-package 'hydra)
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

(straight-use-package 'shut-up)
(straight-use-package 'swiper)
(straight-use-package 'undercover)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(straight-use-package 'zygospore)

(unless running-in-termux
  (straight-use-package 'company)
  (straight-use-package 'counsel)
  (straight-use-package 'find-lisp)
  (straight-use-package 'move-text)

  ;; (add-hook 'after-init-hook 'global-company-mode)
  (move-text-default-bindings)
)

(require 'ido)
;;(if running-in-termux (ido-mode t))

(if (version< emacs-version "25")
      (message "Skipping some modules...")
  (unless running-in-termux
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(provide 'ext-modules)
;;; ext-modules.el ends here
