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
                lsp-enable-symbol-highlighting t
                lsp-headerline-breadcrumb-enable nil
                lsp-lens-enable nil))

  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :config
    (add-hook 'lsp-ui-doc-frame-hook
              (lambda (frame _w)
                (set-face-attribute 'default frame :font "Monospace")))
    :init (setq lsp-ui-sideline-enable t
                lsp-ui-peek-enable t
                lsp-ui-sideline-delay 0.5
                lsp-ui-sideline-update-mode 'line
                lsp-ui-doc-show-with-cursor t))

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

;;
;; Recommended helpers from lsp-mode instructions
;;
(defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; References w/ Role::Role
(defun ccls/references-read () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
    (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
;; (ccls/base 1) direct bases
;; (ccls/derived 1) direct derived
;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
;; (ccls/member 0) => member variables / variables in a namespace
;; (ccls/vars 1) => field
;; (ccls/vars 2) => local variable
;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
;; (ccls/vars 4) => parameter

;; References whose filenames are under this project
;;(lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))

(provide 'c-and-cpp-mode)
;;; c-and-cpp-mode.el ends here
