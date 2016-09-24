;;
;; Packages and Archives
;;

(require 'package)

;; List packages to download
(setq package-list '(evil
		     evil-leader
		     evil-nerd-commenter
		     helm
		     ;;powerline
		     smart-mode-line
		     zenburn-theme
		     smooth-scrolling))

;; Set the archive sources
(setq package-archives '(("elpa"         . "http://tromey.com/elpa/")
			 ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
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

;; - Helm -
(helm-mode 1)

;; - Evil -
(evil-mode 1)
(require 'evil-leader)

;; evil-leader bindings
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)

(evil-leader/set-key "d" 'kill-buffer
		     "f" 'helm-find-files
		     "o" 'helm-mini
		     "b" 'helm-buffers-list
		     "m" 'helm-mini
		     "t" 'split-window-right
		     "w" 'delete-other-windows
		     )

;; - Powerline -
;(require 'powerline)
;(powerline-default-theme)

;; - Smart Mode Line -
(sml/setup)

;; - Smooth Scrolling -
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)
