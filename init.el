(require 'package)

(defvar melpa '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives melpa)
(package-initialize)
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(dolist (p '(bind-key
	     expand-region
	     multiple-cursors
	     expand-region
	     swiper
	     hungry-delete
	     rainbow-mode
	     web-mode
	     yaml-mode
	     gnuplot-mode))
  (package-install p))

(defun user-mc/expand-or-mark-next-symbol ()
  (interactive)
  (if (not (region-active-p))
      (er/mark-symbol)
    (call-interactively #'mc/mark-next-like-this)))
(defun user-mc/expand-or-mark-next-word ()
  (interactive)
  (if (not (region-active-p))
      (er/mark-word)
    (call-interactively #'mc/mark-next-like-this)))

(require 'bind-key)
(require 'multiple-cursors)
(require 'expand-region)
(require 'swiper)
(require 'paredit)
(require 'hungry-delete)

;; source pair
(load-file "~/.emacs.d/sourcepair.el")
(setq sourcepair-source-path '( "." "../*" ))
(setq sourcepair-header-path '( "." "include" "../include" "../*"))

;; Global settings/keys
(setq
 load-prefer-newer t
 backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
 save-place t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(bind-key "RET" 'multiple-cursors-mode mc/keymap)
(bind-keys
 ("C-<down>" . mc/mmlte--down)
 ("C-<up>" . mc/mmlte--up)
 ("M-s" . swiper)
 ("M-v" . split-window-vertically)
 ("M-h" . split-window-horizontally)
 ("M-m" . user-mc/expand-or-mark-next-symbol)
 ("M-M" . user-mc/expand-or-mark-next-word)
 ("M-p" . mark-paragraph)
 ("M-o" . delete-other-windows)
 ("M-'" . mc/mark-all-dwim)
 ("M-c" . comment-region)
 ("M-i" . indent-region)
 ("M-u" . uncomment-region)
 ("M-r" . query-replace)
 ("C-u" . undo)
 ("C-h" . sourcepair-load)
 ("C-t" . hungry-delete-forward))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; case insensitive auto completion
(setq completion-ignore-case  t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Theme stuff
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(menu-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'soft t)

;; file modes
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("28b1e42cbe1c571d52fec3056ea9448ccbd8673c1e2c495df2219da4293da93e" default))
 '(package-selected-packages
   '(scad-mode cmake-mode yaml-mode hungry-delete yasnippet web-mode paredit rainbow-mode swiper multiple-cursors expand-region bind-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
