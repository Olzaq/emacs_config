;;; package --- Summary

;;; Commentary:

;;; Code:

(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "C-c a") (lambda () (interactive) (counsel-ag nil nil "-U --depth 30")))
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
;(global-set-key (kbd "M-x  ") 'counsel-M-x)
(global-set-key (kbd "C-c r") 'counsel-recentf)
(global-set-key (kbd "C-c C-g") 'ag-project)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c A") 'mc/mark-all-like-this)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c c") 'camelscore-word-at-point)
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(fset 'own-add-checkbox
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("- " 0 "%d")) arg)))

(global-set-key (kbd "<f6>") (lambda ()
                               (interactive)
                               (if (string-match "org$" (buffer-name))
                                   (own-add-checkbox)
                                 (find-file-existing (concat org-mode-files-directory "/task_list.org")))))

(global-set-key (kbd "<f7>") (lambda ()
                               (interactive)
                               (if (minibufferp)
                                   (stop-using-minibuffer)
                                 (switch-to-minibuffer-window))))

;; Company
(global-set-key (kbd "C-M-&") 'company-complete-common-or-cycle)

(provide 'own-mappings)
;;; own-mappings.el ends here
