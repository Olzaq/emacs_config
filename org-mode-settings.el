;;; package --- Summary

;;; Commentary:

;;; Code:

(defvar org-mode-files-directory "~/Documents/org-mode/org-files")

(unless (file-directory-p org-mode-files-directory)
  (make-directory org-mode-files-directory t))

(defvar org-agenda-files
      (find-lisp-find-files org-mode-files-directory "\\.org$"))

(defvar org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "C-c l") 'org-metaleft)
(define-key org-mode-map (kbd "C-c r") 'org-metaright)


(provide 'org-mode-settings)
;;; org-mode-settings.el ends here
