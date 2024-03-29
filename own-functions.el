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
(defun CAPITAL (s) "Convert S to CAPITAL_STYLE."
  (mapconcat 'upcase   (split-name s) "_"))
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
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)            (dasherize  s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s)            (camelCase  s))
	    ((string-match-p "\\b[a-z]+\\(?:[A-Z][a-z]+\\)+" s)     (CamelCase  s))
	    ((string-match-p "\\(?:[A-Z][a-z]+::\\)+[A-Z][a-z]+" s) (underscore s))
        ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)            (CAPITAL    s))
        ((string-match-p "\\(?:[A-Z]+\\)+$"  s)                 (colonize   s)) ))

(defun prompt-for-insert ()
  "Prompt for inserting text."
  (interactive)
  (insert (read-string "Insert: ")))

(defun eval-and-replace ()
  "Evaluate exp and replace it with result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'ARG'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun extract-to-package (name start end)
  "Create buffer NAME.el and exctract lines between START and END."
  (interactive (list (read-string "Package name to create: ")
                     (region-beginning) (region-end)))

  (let* ((snip (buffer-substring start end))
        (p-name (concat name ".el"))
        (header-lines
         (concat
          ";;; package --- Summary\n\n"
          ";;; Commentary:\n\n"
          ";;; Code:\n\n"))
        (footer-lines
         (concat ";;; " p-name " ends here\n")))
    (with-current-buffer (find-file-noselect p-name)
      (insert header-lines)
      (insert snip)
      (insert (format "\n(provide '%s)\n" name))
      (insert footer-lines)
      (save-buffer))
    (delete-region start end)
    (insert (format "(require '%s)\n" name))))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun stop-using-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(defun show-file-name-and-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun toggle-window-dedicated ()
  "Control is Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (condition-case nil
          (delete-frame)
        (error (save-buffers-kill-emacs)))
    (message nil)))

(provide 'own-functions)
;;; own-functions.el ends here
