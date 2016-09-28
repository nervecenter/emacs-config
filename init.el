;;
;; Packages and Archives
;;

(require 'package)

;; List packages to download
(setq package-list '(evil
             evil-leader
             evil-nerd-commenter
             ido
             ido-ubiquitous
             ido-better-flex
             ido-vertical-mode
             smart-mode-line
             smooth-scrolling
             zenburn-theme
	     relative-line-numbers
	     rainbow-delimiters))

;; Set the archive sources
(setq package-archives '(("elpa"         . "https://tromey.com/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")
                         ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;
;; Emacs Settings
;;

(set-face-attribute 'default t :font "DejaVu Sans Mono for Powerline")
(load-theme 'zenburn t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)

;;
;; Package Settings
;;

;; - RecentF -
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; - Ido -
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; - Evil -

;; evil-leader bindings
(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader "<SPC>")
(evil-mode nil)
(global-evil-leader-mode)
(evil-mode 1)
(kill-buffer "*Messages*")

(evil-leader/set-key
  "d" 'kill-buffer
  "f" 'find-file
  "b" 'ido-switch-buffer
  "r" 'ido-recentf-open
  "t" 'split-window-right
  "w" 'delete-other-windows
  "n" 'previous-buffer
  "m" 'next-buffer
  )

;; - Smart Mode Line -
(sml/setup)

;; - Rainbow Delimiters -
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; - Relative Line Numbers -
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)

;; - Smooth Scrolling -
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

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
