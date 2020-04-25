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
(global-set-key (kbd "C-x g") 'ag-project)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c A") 'mc/mark-all-like-this)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c c") 'camelscore-word-at-point)

(provide 'own-mappings)
;;; own-mappings.el ends here
