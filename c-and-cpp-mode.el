;;; package --- Summary

;;; Commentary:

;;; Code:

;; C-mode setting
(defconst bsd-style-extend
  '("bsd"
    (c-basic-offset 4)
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "bsd-style-extend" bsd-style-extend)

(setq c-default-style "bsd-style-extend")

(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

(defun do-irony-config ()
  "Irony config."
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(defun do-lsp-mode-config ()
  "LSP mode config."
  (use-package lsp-mode
    :commands lsp
    :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
    :init (setq lsp-modeline-diagnostics-enable nil
                lsp-modeline-code-actions-enable nil))

  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :init (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-sideline-update-mode 'line))

  (use-package company-lsp
    :after lsp-mode
    :commands company-lsp)

  (use-package ccls
    :after lsp-mode
    :config (setq ccls-executable "~/src/CCLS_install/bin/ccls")
    :hook ((c-mode c++-mode objc-mode cuda-mode) .
           (lambda () (require 'ccls) (lsp))))

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(if use-lsp-mode
    (do-lsp-mode-config)
  (do-irony-config))

(provide 'c-and-cpp-mode)
;;; c-and-cpp-mode.el ends here
