;;; package --- Summary

;;; Commentary:

;;; Code:

; Make sure org is properly installed
(add-to-list 'load-path "~/.emacs_modules/org-mode/lisp")
(unless (file-readable-p (file-truename "~/.emacs_modules/org-mode/lisp/org-loaddefs.el"))
  (error "Org-mode not properly installed.  Run 'make autoloads'"))
(require 'org-loaddefs)
(require 'org)

(defun byte-recompile-dir-exclude (dir &optional pattern)
  "Bytecompile files in DIR unless match PATTERN."
  (mapc
   (lambda (file)
     (unless (if pattern
                 (string-match-p pattern file))
       (byte-recompile-file file nil 0)))
   (directory-files dir t "\.el$")))

(add-to-list 'load-path "~/.emacs_modules/ag.el")
(add-to-list 'load-path "~/.emacs_modules/cmake-ide")
(add-to-list 'load-path "~/.emacs_modules/company-irony")
(add-to-list 'load-path "~/.emacs_modules/company-lsp")
(add-to-list 'load-path "~/.emacs_modules/company-mode")
(add-to-list 'load-path "~/.emacs_modules/csv-mode")
(add-to-list 'load-path "~/.emacs_modules/dash.el")
(add-to-list 'load-path "~/.emacs_modules/dockerfile-mode")
(add-to-list 'load-path "~/.emacs_modules/editorconfig-emacs")
(add-to-list 'load-path "~/.emacs_modules/emacs-async")
(add-to-list 'load-path "~/.emacs_modules/emacs-ccls")
(add-to-list 'load-path "~/.emacs_modules/expand-region")
(add-to-list 'load-path "~/.emacs_modules/f.el")
(add-to-list 'load-path "~/.emacs_modules/flycheck")
(add-to-list 'load-path "~/.emacs_modules/helm")
(add-to-list 'load-path "~/.emacs_modules/helm-gtags")
(add-to-list 'load-path "~/.emacs_modules/ht.el")
(add-to-list 'load-path "~/.emacs_modules/hydra.git")
(add-to-list 'load-path "~/.emacs_modules/irony-mode")
(add-to-list 'load-path "~/.emacs_modules/levenshtein")
(add-to-list 'load-path "~/.emacs_modules/lsp-mode")
(add-to-list 'load-path "~/.emacs_modules/lsp-ui")
(add-to-list 'load-path "~/.emacs_modules/magit/lisp")
(add-to-list 'load-path "~/.emacs_modules/markdown-mode")
(add-to-list 'load-path "~/.emacs_modules/move-text")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")
(add-to-list 'load-path "~/.emacs_modules/rtags/src")
(add-to-list 'load-path "~/.emacs_modules/s.el")
(add-to-list 'load-path "~/.emacs_modules/seq")
(add-to-list 'load-path "~/.emacs_modules/shut-up")
(add-to-list 'load-path "~/.emacs_modules/spinner")
(add-to-list 'load-path "~/.emacs_modules/swiper")
(add-to-list 'load-path "~/.emacs_modules/transient/lisp")
(add-to-list 'load-path "~/.emacs_modules/undercover.el")
(add-to-list 'load-path "~/.emacs_modules/with-editor")
(add-to-list 'load-path "~/.emacs_modules/zygospore.el")

(unless running-in-termux
  (byte-recompile-dir-exclude "~/.emacs_modules/dash.el")
  (byte-recompile-directory "~/.emacs_modules/editorconfig-emacs" 0)
  (byte-recompile-file "~/.emacs_modules/f.el/f.el" nil 0)
  (byte-recompile-directory "~/.emacs_modules/helm" 0)
  (byte-recompile-directory "~/.emacs_modules/helm-gtags" 0)
  (byte-recompile-file "~/.emacs_modules/ht.el/ht.el" nil 0)
  (byte-recompile-directory "~/.emacs_modules/hydra.git" 0)
  (if (version< emacs-version "25")
      (message "Skipping bytecompile...")
    (byte-recompile-directory "~/.emacs_modules/ag.el" 0)
    (byte-recompile-directory "~/.emacs_modules/s.el" 0)
    (byte-recompile-directory "~/.emacs_modules/seq" 0)
    (byte-recompile-directory "~/.emacs_modules/swiper" 0)
    (byte-recompile-directory "~/.emacs_modules/transient" 0)

    ;; Skip magit-libgit.el compilation
    (byte-recompile-dir-exclude "~/.emacs_modules/magit/lisp" "magit-libgit.el"))

  ;;(byte-recompile-directory "~/.emacs_modules/cmake-ide" 0)
  (byte-recompile-directory "~/.emacs_modules/company-lsp" 0)
  (byte-recompile-directory "~/.emacs_modules/company-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/company-irony" 0)
  (byte-recompile-directory "~/.emacs_modules/csv-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/emacs-async" 0)
  (byte-recompile-directory "~/.emacs_modules/emacs-ccls" 0)
  (byte-recompile-directory "~/.emacs_modules/expand-region" 0)
  (byte-recompile-directory "~/.emacs_modules/irony-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/levenshtein" 0)
  ;;(byte-recompile-directory "~/.emacs_modules/lsp-mode" 0)
  ;;(byte-recompile-directory "~/.emacs_modules/lsp-ui" 0)
  (byte-recompile-directory "~/.emacs_modules/markdown-mode" 0)
  (byte-recompile-directory "~/.emacs_modules/move-text" 0)
  (byte-recompile-directory "~/.emacs_modules/org-mode/lisp" 0)
  (byte-recompile-directory "~/.emacs_modules/spinner" 0)
  (byte-recompile-directory "~/.emacs_modules/with-editor" 0)
  (byte-recompile-directory "~/.emacs_modules/zygospore.el" 0)

  (require 'ag)
  (require 'cmake-ide)
  (require 'company)
  (require 'counsel)
  (require 'dockerfile-mode)
  (require 'editorconfig)
  (require 'ccls)
  (require 'expand-region)
  (require 'find-lisp)
  (require 'irony)
  (require 'irony-cdb)
  (require 'lsp-mode)
  (require 'rtags)
  (require 'move-text)
  (require 'multiple-cursors)
  (require 'swiper)

  (set-variable 'rtags-path (file-truename "~/.emacs_modules/rtags/bin"))
  (cmake-ide-setup)
  (editorconfig-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (move-text-default-bindings)

  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t)
)

(require 'ido)
(require 'zygospore)
;;(if running-in-termux (ido-mode t))

(if (version< emacs-version "25")
      (message "Skipping some modules...")
  (unless running-in-termux
    (require 'flycheck)
    (add-hook 'after-init-hook #'global-flycheck-mode))
    (require 'git-settings))


(provide 'ext-modules)
;;; ext-modules.el ends here
