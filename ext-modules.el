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
(add-to-list 'load-path "~/.emacs_modules/editorconfig-emacs")
(add-to-list 'load-path "~/.emacs_modules/emacs-request")
(add-to-list 'load-path "~/.emacs_modules/f.el")
(add-to-list 'load-path "~/.emacs_modules/flycheck")
(add-to-list 'load-path "~/.emacs_modules/hydra.git")
(add-to-list 'load-path "~/.emacs_modules/irony-mode")
(add-to-list 'load-path "~/.emacs_modules/magit/lisp")
(add-to-list 'load-path "~/.emacs_modules/magit-gerrit")
(add-to-list 'load-path "~/.emacs_modules/multiple-cursors.el")
(add-to-list 'load-path "~/.emacs_modules/org-jira")
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
  (byte-recompile-directory "~/.emacs_modules/magit-gerrit" 0)
  (byte-recompile-directory "~/.emacs_modules/org-jira" 0)
  (byte-recompile-directory "~/.emacs_modules/org-mode/lisp" 0)
  (byte-recompile-directory "~/.emacs_modules/s.el" 0)
  (byte-recompile-directory "~/.emacs_modules/seq" 0)
  (byte-recompile-directory "~/.emacs_modules/swiper" 0)
  (byte-recompile-directory "~/.emacs_modules/transient" 0)
  )

(byte-recompile-directory "~/.emacs_modules/company-mode" 0)
(byte-recompile-directory "~/.emacs_modules/company-irony" 0)
;(byte-recompile-directory "~/.emacs_modules/emacs-request" 0)
(byte-recompile-directory "~/.emacs_modules/irony-mode" 0)
(byte-recompile-directory "~/.emacs_modules/with-editor" 0)

(require 'ag)
(require 'company)
(require 'counsel)
(require 'editorconfig)
(require 'find-lisp)
(require 'ido)
(require 'irony)
(require 'irony-cdb)
(require 'multiple-cursors)
(require 'server)
(require 'swiper)

(if (version< emacs-version "25")
    (message "Skipping some modules...")
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (require 'flycheck)
  (require 'magit)

  (setq jiralib-url "http://jira-host-todo/jira")
  (require 'org-jira)
  (require 'magit-gerrit)
)

(editorconfig-mode 1)
(ido-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(unless (server-running-p) (server-start))

(provide 'ext-modules)
;;; ext-modules.el ends here
