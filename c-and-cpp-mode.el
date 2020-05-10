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

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(provide 'c-and-cpp-mode)
;;; c-and-cpp-mode.el ends here
