;;; package --- Summary

;;; Commentary:

;;; Code:

(defun to-underscore () (interactive)
    (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil
    (region-beginning) (region-end)) (downcase-region (region-beginning) (region-end)))
)

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun capitalize-all-but-first (l)
  (let ((first (car l))
        (rest (cdr l)))
    (cons first (mapcar 'capitalize rest)) ))

(defun camelCase  (s) (mapconcat 'identity (capitalize-all-but-first (split-name s)) ""))
(defun CamelCase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (camelscore txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))

(defun camelscore (s)
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	        (dasherize  s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	        (camelCase  s))
	    ((string-match-p "\\b[a-z]+\\(?:[A-Z][a-z]+\\)+" s)	    (CamelCase  s))
	    ((string-match-p "\\(?:[A-Z][a-z]+::\\)+[A-Z][a-z]+" s)	(underscore s))
        ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	        (colonize   s)) ))


(provide 'own-functions)
;;; own-functions.el ends here
