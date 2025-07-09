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
             gnuplot-mode
             scad-mode))
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

;; properly recognize arrow keys
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])

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
 ("C-t" . hungry-delete-forward)
 )

;; delete trailing whitespace on save
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'hexl-mode)
              (delete-trailing-whitespace))))

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

;; org mode
(add-hook 'org-mode-hook 'org-indent-mode)

;; redirect custom bullshit to a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; always follow git symlinks
(setq vc-follow-symlinks t)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tabs-width 4)
