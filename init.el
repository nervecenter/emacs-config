;; Set custom file and load for override
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

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
(eval-when-compile
  (require 'use-package))

;;
;; Emacs Settings
;;

;; Set font, bigger for high-DPI displays
(if (<= (display-pixel-width) 1080)
    (add-to-list 'default-frame-alist
                 '(font . "Fira Mono-10")
				 ;'(font . "DejaVu Sans Mono for Powerline-10")
				 )
  (add-to-list 'default-frame-alist
               '(font . "Fira Mono-11")
			   ;'(font . "DejaVu Sans Mono for Powerline-11")
			   ))
;; Turn off toolbar, scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Highlight matching parens, highlight current line
(show-paren-mode t)
(hl-line-mode t)
;; Move between windows with Shift+<Arrow keys>
(windmove-default-keybindings)
;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (format "%s/%s%s/"
		  temporary-file-directory
		  "emacs"
		  (user-uid)))
;; Quantities
(setq-default
 indent-tabs-mode  nil
 tab-width         4
 indent-tabs-mode  0
 c-basic-offset    4
 )
(setq
 indent-line-function            'insert-tab
 inhibit-splash-screen           t
 initial-scratch-message         nil
 initial-major-mode              'text-mode
 backup-directory-alist          `((".*" . ,emacs-tmp-dir))
 auto-save-file-name-transforms  `((".*" ,emacs-tmp-dir t))
 auto-save-list-file-prefix      emacs-tmp-dir
 )



;;
;; Package Settings
;;


;; Theme Packages

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   ;;(load-theme 'solarized t)
;;   (load-theme 'solarized-light t))

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
  (use-package ido-completing-read
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
  (use-package evil-nerd-commenter
	:ensure t)
  (use-package evil-leader
    :ensure t
    :config
	(defun revert-buffer-no-confirm ()
	  "Revert buffer without confirmation."
	  (interactive)
	  (revert-buffer :ignore-auto :noconfirm)
	  (message "Updated buffer."))
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
      "o" 'other-window
      "u" 'revert-buffer-no-confirm
	  ;; Nerd Commenter bindings
	  "cc" 'evilnc-comment-or-uncomment-lines
	  ;; "cr" 'comment-or-uncomment-region
	  ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
    ))
  (evil-mode nil)
  (global-evil-leader-mode)
  (evil-mode 1)
  (kill-buffer "*Messages*")
  (add-to-list 'evil-emacs-state-modes 'term-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq column-number-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  (setq tab-width 4
		truncate-lines 0
		c-basic-offset 4
		indent-tabs-mode 0))

(defun lisp-like-settings ()
  (setq tab-width 2
		truncate-lines 0
		lisp-basic-offset 2
		indent-tabs-mode 0))

;; To convert tabs to spaces, M-x untabify


;; Language Modes

(use-package d-mode
  :ensure t
  :config
  (add-hook 'd-mode-hook 'c-like-settings))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'lisp-like-settings))

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook 'lisp-like-settings))

(use-package haskell-mode
  :ensure t)

(use-package adoc-mode
  :ensure t)

