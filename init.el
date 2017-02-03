;;
;; Packages
;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Managing packages with use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Emacs Settings
;;

(if (<= (display-pixel-width) 1080)
;;    (add-to-list 'default-frame-alist
;;                 '(font . "DejaVu Sans Mono for Powerline-10"))
;;  (add-to-list 'default-frame-alist
;;               '(font . "DejaVu Sans Mono for Powerline-11"))
    (add-to-list 'default-frame-alist
                 '(font . "Fira Mono-10"))
  (add-to-list 'default-frame-alist
               '(font . "Fira Mono-11"))
  )
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(hl-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(windmove-default-keybindings)
(setq-default indent-tabs-mode 0)
(setq-default c-basic-offset 4)
;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (format "%s/%s%s/"
		  temporary-file-directory
		  "emacs"
		  (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;;
;; Package Settings
;;


;; Theme Packages

(use-package zenburn-theme
  :ensure t
  :config
  ;;(load-theme 'zenburn t)
  )

(use-package solarized-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; Utility Packages

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-exclude '("^/var/folders\\.*"
                         "COMMIT_EDITMSG\\'"
                         ".*-autoloads\\.el\\'"
                         "ido\\.last\\'"
                         "[/\\]\\.elpa/"
                         )))

(use-package ido
  :config
  (use-package ido-ubiquitous
    :ensure t)
  ;;(use-package ido-better-flex
  ;;  :ensure t)
  (use-package ido-vertical-mode
    :ensure t)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-mode 1)
  (ido-vertical-mode 1)
  ;;(ido-better-flex/enable)

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))

(use-package evil
  :ensure t
  :config
  (use-package evil-leader
    :ensure t
    :config
    (setq evil-leader/in-all-states 1)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "d" 'kill-buffer
      "f" 'find-file
      "b" 'ido-switch-buffer
      "r" 'ido-recentf-open
      "t" 'split-window-right
      "w" 'delete-other-windows
      "n" 'previous-buffer
      "m" 'next-buffer
    ))
  (evil-mode nil)
  (global-evil-leader-mode)
  (evil-mode 1)
  (kill-buffer "*Messages*"))


(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq column-number-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;(use-package nlinum-relative
;;  :ensure t
;;  :config
;;  (nlinum-relative-setup-evil)
;;  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;  (setq nlinum-relative-redisplay-delay 0
;;        nlinum-relative-current-symbol ""
;;        nlinum-relative-offset 0))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 5
        scroll-conservatively 9999
        scroll-step 1
		mouse-wheel-scroll-amount '(3 ((shift) . 3))
		mouse-wheel-progressive-speed nil))

(use-package paredit
  :ensure t)

(use-package org
  :ensure t)


;; Language settings for hooks

(defun c-like-settings ()
  (setq tab-width 4)
  (setq truncate-lines 0)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode 0))

(defun lisp-like-settings ()
  (setq tab-width 2))

;; To convert tabs to spaces, M-x untabify


;; Language Modes

(use-package d-mode
  :ensure t
  :config
  (add-hook 'd-mode-hook 'c-like-settings))

(use-package haskell-mode
  :ensure t)

(use-package adoc-mode
  :ensure t)

;;
;; Keybinds
;;

;(dolist (key '("\C-n" "\C-m" "\C-p"))
;  (global-unset-key key))

;(global-set-key (kbd "\C-n") 'previous-buffer)
;(global-set-key (kbd "\C-m") 'next-buffer)
;(global-set-key (kbd "\C-p") 'ido-recentf-open)

;; - Emacs-w64 and Emacs Mac -

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mac-mouse-wheel-smooth-scroll nil)
 '(package-selected-packages
   (quote
	(rainbow-delimiters relative-line-numbers zenburn-theme smooth-scrolling smart-mode-line ido-vertical-mode ido-better-flex ido-ubiquitous evil-nerd-commenter evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
