;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'gud)

; invoke
(global-set-key [f8] 'gdb)

; GDB layout
(defadvice gdb-setup-windows (after activate)
  (gdb-setup-my-windows))

(defun gdb-setup-my-windows ()
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let
      ((win0 (selected-window))             ; breakpoints
       (win1 (split-window-horizontally
              (floor (* 0.5 (window-width)))))   ; source + i/o
       (win2 (split-window-vertically
              (floor (* 0.5 (window-body-height))))) ; gdb
       (win3 (split-window-vertically
              (floor (* 0.5 (window-body-height))))) ; locals
       (win4 (split-window-vertically
              (floor (* 0.6 (window-body-height))))) ; stack
       )
    (select-window win1)
    ;; configurating right window
    (let
        ((winSrc (selected-window)) ; source
         (winIO (split-window-vertically (floor (* 0.9 (window-body-height))))) ; I/O
         )
      (set-window-buffer winIO (gdb-get-buffer-create 'gdb-inferior-io))
      (set-window-buffer
       winSrc
       (if gud-last-last-frame
           (gud-find-file (car gud-last-last-frame))
         (if gdb-main-file
             (gud-find-file gdb-main-file)
           (list-buffers-noselect))))
      (setq gdb-source-window winSrc)
      (set-window-dedicated-p winIO t))

    (set-window-buffer win0 (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-buffer win3 (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-buffer win4 (gdb-get-buffer-create 'gdb-stack-buffer))
    (select-window win2)))

; GDB variables
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-show-changed-values t)
(setq gdb-use-colon-colon-notation t)
(setq gdb-use-separate-io-buffer nil)
(setq gdb-delete-out-of-scope t)
(setq gdb-speedbar-auto-raise t)

(add-to-list 'display-buffer-alist
             (cons 'cdb-source-code-buffer-p
                   (cons 'display-source-code-buffer nil)))

(defun cdb-source-code-buffer-p (bufName action)
  "Return whether BUFNAME is a source code buffer."
  (let ((buf (get-buffer bufName)))
    (and buf
         (with-current-buffer buf
           (derived-mode-p buf 'c++-mode 'c-mode 'csharp-mode 'nxml-mode)))))

(defun display-source-code-buffer (sourceBuf alist)
  "Find a window with source code and set SOURCEBUF inside it."
  (let* ((curbuf (current-buffer))
     (wincurbuf (get-buffer-window curbuf))
     (win (if (and wincurbuf
                   (derived-mode-p sourceBuf 'c++-mode 'c-mode 'nxml-mode)
                   (derived-mode-p (current-buffer) 'c++-mode 'c-mode 'nxml-mode))
              wincurbuf
            (get-window-with-predicate
             (lambda (window)
               (let ((bufName (buffer-name (window-buffer window))))
                 (or (cdb-source-code-buffer-p bufName nil)
                     (assoc bufName display-buffer-alist)
                     ))))))) ;; derived-mode-p doesn't work inside this, don't know why...
    (set-window-buffer win sourceBuf)
    win))

(provide 'gdb-settings)
;;; gdb-settings.el ends here
