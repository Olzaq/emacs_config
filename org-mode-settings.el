;;; package --- Summary

;;; Commentary:

;;; Code:

(setq org-agenda-files
      (find-lisp-find-files "~/Documents/org-mode/org-files" "\\.org$"))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "ESC-<left>") 'org-metaleft)


(provide 'org-mode-settings)
;;; org-mode-settings.el ends here
