;;; package --- Summary

;;; Commentary:

;;; Code:

;; Helm setup

(use-package helm
  :preface
  (require 'helm-grep)
  :demand t
  :bind (("M-x" . helm-M-x)
         ;;("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x c o" . helm-occur)
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-filtered-bookmarks)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         :map shell-mode-map
         ("C-c C-l" . helm-comint-input-ring) ; in shell mode
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history)
         )
  :config (helm-mode 1))

;(global-set-key (kbd "C-c h") 'helm-command-prefix)

(add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))

(provide 'helm-settings)
;;; helm-settings.el ends here
