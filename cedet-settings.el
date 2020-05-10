;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'cc-mode)
(require 'semantic)

;; To enable more advanced functionality for name completion, etc.,
;(require 'semantic/ia)

;(require 'semantic/bovine/gcc)

(global-semanticdb-minor-mode 1)
;(global-semantic-mru-bookmark-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-summary-mode 1)


(defun my-semantic-hook ()
  "Semantic hook."
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  "Config cedet hook."
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode t)

(provide 'cedet-settings)
;;; cedet-settings.el ends here
