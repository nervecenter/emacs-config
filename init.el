;;
;; Packages and Archives
;;

(require 'package)

;; List packages to download
(setq package-list '(evil
		     evil-leader
		     evil-nerd-commenter
		     helm
		     powerline))

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
;; Package Settings
;;

(evil-mode 1)

;; evil-leader bindings
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)

(evil-leader/set-key "d" 'kill-buffer
		     "f" 'find-file
		     ;;"o" recent files
		     "b" 'switch-to-buffer
		     ;;"m" buffers and recents mixed
		     ;;"t" split window
		     ;;"w" close all windows but current
		     )

;; Powerline settings
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)
