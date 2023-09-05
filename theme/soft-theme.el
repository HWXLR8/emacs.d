;;; soft-theme.el --- soft
;;; Version: 1.0
;;; Commentary:
;;; A theme called soft
;;; Code:

(deftheme soft "DOCSTRING for soft")
(custom-theme-set-faces 'soft
			'(default ((t (:foreground "#a89898" :background "#000000" ))))
			'(cursor ((t (:background "#a89898" ))))
			'(fringe ((t (:background "#282828" ))))
			'(mode-line ((t (:foreground "#282828" :background "#837c7f" ))))
			'(region ((t (:background "#1a1a1a" ))))
			'(secondary-selection ((t (:background "#3e3834" ))))
			'(font-lock-builtin-face ((t (:foreground "#ffb375" ))))
			'(font-lock-comment-face ((t (:foreground "#7c6f64" ))))
			'(font-lock-function-name-face ((t (:foreground "#b8bb26" ))))
			'(font-lock-keyword-face ((t (:foreground "#fb4934" ))))
			'(font-lock-string-face ((t (:foreground "#c1b88a" ))))
			'(font-lock-type-face ((t (:foreground "#d3869b" ))))
			'(font-lock-constant-face ((t (:foreground "#d3869b" ))))
			'(font-lock-variable-name-face ((t (:foreground "#83a598" ))))
			'(minibuffer-prompt ((t (:foreground "#b8bb26" :bold t ))))
			'(font-lock-warning-face ((t (:foreground "red" :bold t ))))
			)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'soft)

;;; soft-theme.el ends here
