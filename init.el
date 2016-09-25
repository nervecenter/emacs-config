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
             zenburn-theme))

;; Set the archive sources
(setq package-archives '(("elpa"         . "http://tromey.com/elpa/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
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
(evil-mode 1)
(require 'evil-leader)

;; evil-leader bindings
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)

(evil-leader/set-key "d" 'kill-buffer
                     "f" 'find-file
                     "b" 'ido-switch-buffer
             "r" 'ido-recentf-open
             "t" 'split-window-right
             "w" 'delete-other-windows
             )

;; - Smart Mode Line -
(sml/setup)

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
