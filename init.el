;;
;; Packages
;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Emacs Settings
;;

(set-face-attribute 'default t :font "DejaVu Sans Mono for Powerline-11")
(load-theme 'zenburn t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(hl-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;
;; Package Settings
;;

;; - RecentF -
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))

;; - Ido -
(use-package ido
  :config
  (use-package ido-ubiquitous
    :ensure t)
  (use-package ido-better-flex
    :ensure t)
  (use-package ido-vertical-mode
    :ensure t)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-mode 1)
  (ido-vertical-mode 1)

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))

;; - Evil -

;; evil-leader bindings
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


;; - Smart Mode Line -
(use-package smart-mode-line
  :ensure t
  :init
  (sml/setup))

;; - Rainbow Delimiters -
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; - nlinum -
(use-package nlinum
  :ensure t
  :init
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  :config
  (setq nlinum-relative-redisplay-delay 0
        nlinum-relative-current-symbol "")
        nlinum-relative-offset 0)

;; - Smooth Scrolling -
(use-package smooth-scrolling
  :ensure t
  :init
  (setq scroll-margin 5
        scroll-conservatively 9999
        scroll-step 1))

;;
;; Keybinds
;;

;(dolist (key '("\C-n" "\C-m" "\C-p"))
;  (global-unset-key key))

;(global-set-key (kbd "\C-n") 'previous-buffer)
;(global-set-key (kbd "\C-m") 'next-buffer)
;(global-set-key (kbd "\C-p") 'ido-recentf-open)

;; - Emacs-w64 -

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters relative-line-numbers zenburn-theme smooth-scrolling smart-mode-line ido-vertical-mode ido-better-flex ido-ubiquitous evil-nerd-commenter evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
