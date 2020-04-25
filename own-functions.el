;;; package --- Summary

;;; Commentary:

;;; Code:

(defun to-underscore ()
  "Separate word at point with '_' at each capital letter."
  (interactive)
    (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil
    (region-beginning) (region-end)) (downcase-region (region-beginning) (region-end)))
)

(defun split-name (s)
  "Split given string S at each lower-to-upper-case boundary."
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun capitalize-all-but-first (l)
  "Capitalize all words in L expect the first one."
  (let ((first (car l))
        (rest (cdr l)))
    (cons first (mapcar 'capitalize rest)) ))

(defun camelCase  (s) "Convert S to camelCaseStyle."
  (mapconcat 'identity (capitalize-all-but-first (split-name s)) ""))
(defun CamelCase  (s) "Convert S to CamelCaseStyle."
  (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) "Convert S to under_score_style."
  (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) "Convert S to dash-style."
  (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) "Convert S to Scope::Resolution::Style."
  (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore-word-at-point ()
  "Cycle naming styles."
  (interactive)
  (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (camelscore txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))

(defun camelscore (s)
  "Select next matching style for given string S."
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	        (dasherize  s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)	        (camelCase  s))
	    ((string-match-p "\\b[a-z]+\\(?:[A-Z][a-z]+\\)+" s)	    (CamelCase  s))
	    ((string-match-p "\\(?:[A-Z][a-z]+::\\)+[A-Z][a-z]+" s)	(underscore s))
        ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	        (colonize   s)) ))


(provide 'own-functions)
;;; own-functions.el ends here
