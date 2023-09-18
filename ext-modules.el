;;; package --- Summary

;;; Commentary:

;;; Code:

(straight-use-package 'ag)

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config (autoload 'csv-mode "csv-mode"
            "Major mode for editing comma-separated value files." t))

(straight-use-package 'dash)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(straight-use-package 'emacs-async)
(straight-use-package 'expand-region)
(straight-use-package 'f)

(unless running-in-termux
  (straight-use-package 'flycheck))

(straight-use-package 'ht)
(straight-use-package 'hydra)
(straight-use-package 'levenshtein)
(straight-use-package 'markdown-mode)
(straight-use-package 'multiple-cursors)
(straight-use-package 's)
(straight-use-package 'seq)
(straight-use-package 'spinner)
(straight-use-package 'shut-up)
(straight-use-package 'swiper)
(straight-use-package 'undercover)

(use-package yasnippet
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
