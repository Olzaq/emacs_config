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
                lsp-modeline-workspace-status-enable nil
                lsp-modeline-code-actions-enable nil
                lsp-enable-symbol-highlighting nil
                lsp-headerline-breadcrumb-enable nil
                lsp-lens-enable nil))

  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :config
    (add-hook 'lsp-ui-doc-frame-hook
              (lambda (frame _w)
                (set-face-attribute 'default frame :font "Overpass Mono 11")))
    :init (setq lsp-ui-sidelien-enable t
                lsp-ui-peek-enable nil
                lsp-ui-sideline-delay 0.5
                lsp-ui-sideline-update-mode 'line
                lsp-ui-doc-show-with-cursor nil))

  (use-package company-lsp
    :after (lsp-mode company)
    :commands company-lsp
    :config
    (setq company-lsp-enable-snippet t
          company-lsp-cache-candidates t)
    (push 'company-lsp company-backends))

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
