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


;; - Emacs-w64 and Emacs Mac -

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
	("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
	(solarized-color-blend it "#fdf6e3" 0.25)
	(quote
	 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
	(("#eee8d5" . 0)
	 ("#B4C342" . 20)
	 ("#69CABF" . 30)
	 ("#69B7F0" . 50)
	 ("#DEB542" . 60)
	 ("#F2804F" . 70)
	 ("#F771AC" . 85)
	 ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
	("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
	("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
	(use-package rainbow-delimiters relative-line-numbers zenburn-theme smooth-scrolling smart-mode-line ido-vertical-mode ido-better-flex ido-ubiquitous evil-nerd-commenter evil-leader evil)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
	(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
