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
(add-to-list 'load-path "~/.emacs_modules/cmake-ide")
(add-to-list 'load-path "~/.emacs_modules/company-irony")
(add-to-list 'load-path "~/.emacs_modules/company-mode")
(add-to-list 'load-path "~/.emacs_modules/csv-mode")
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
(add-to-list 'load-path "~/.emacs_modules/levenshtein")
(add-to-list 'load-path "~/.emacs_modules/magit/lisp")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")
(add-to-list 'load-path "~/.emacs_modules/rtags/src")
(add-to-list 'load-path "~/.emacs_modules/s.el")
(add-to-list 'load-path "~/.emacs_modules/seq")
(add-to-list 'load-path "~/.emacs_modules/shut-up")
(add-to-list 'load-path "~/.emacs_modules/swiper")
(add-to-list 'load-path "~/.emacs_modules/transient/lisp")
(add-to-list 'load-path "~/.emacs_modules/undercover.el")
(add-to-list 'load-path "~/.emacs_modules/with-editor")
(add-to-list 'load-path "~/.emacs_modules/zygospore.el")

(unless running-in-termux
  ;;(byte-recompile-directory "~/.emacs_modules/dash.el" 0)
  (byte-recompile-directory "~/.emacs_modules/editorconfig-emacs" 0)
  (if (version< emacs-version "26")
      (message "Skipping bytecompile: f.el")
    ;;(byte-recompile-directory "~/.emacs_modules/f.el" 0)
    )
  (byte-recompile-directory "~/.emacs_modules/helm" 0)
  (byte-recompile-directory "~/.emacs_modules/helm-gtags" 0)
  (byte-recompile-directory "~/.emacs_modules/hydra.git" 0)
  (if (version< emacs-version "25")
    (message "Skipping bytecompile...")
    (byte-recompile-directory "~/.emacs_modules/ag.el" 0)
    (byte-recompile-directory "~/.emacs_modules/s.el" 0)
    (byte-recompile-directory "~/.emacs_modules/seq" 0)
    (byte-recompile-directory "~/.emacs_modules/swiper" 0)
    (byte-recompile-directory "~/.emacs_modules/transient" 0)

    ;; Skip magit-libgit.el compilation
    (mapc
     (lambda (file)
       (unless (string-match-p "magit-libgit.el" file)
         (byte-recompile-file file nil 0)))
     (directory-files "~/.emacs_modules/magit/lisp" t "\.el$"))
    )

  ;;(byte-recompile-directory "~/.emacs_modules/cmake-ide" 0)
  (byte-recompile-directory "~/.emacs_modules/company-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/company-irony" 0)
  (byte-recompile-directory "~/.emacs_modules/csv-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/expand-region" 0)
  (byte-recompile-directory "~/.emacs_modules/irony-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/levenshtein" 0)
  (byte-recompile-directory "~/.emacs_modules/org-mode/lisp" 0)
  (byte-recompile-directory "~/.emacs_modules/with-editor" 0)
  (byte-recompile-directory "~/.emacs_modules/zygospore.el" 0)

  (require 'ag)
  (require 'cmake-ide)
  (require 'company)
  (require 'counsel)
  (require 'dockerfile-mode)
  (require 'editorconfig)
  (require 'expand-region)
  (require 'find-lisp)
  (require 'irony)
  (require 'irony-cdb)
  (require 'rtags)
  (require 'multiple-cursors)
  (require 'swiper)
  (require 'zygospore)

  (if (version< emacs-version "25")
      (message "Skipping some modules...")
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (require 'flycheck)
    (require 'git-settings)
    )

  (set-variable 'rtags-path (file-truename "~/.emacs_modules/rtags/bin"))
  (cmake-ide-setup)
  (editorconfig-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))

  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t)
)

(require 'ido)
(if running-in-termux (ido-mode t))

(provide 'ext-modules)
;;; ext-modules.el ends here
