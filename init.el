;;
;; Packages and Archives
;;

(require 'package)

;; Set the archive sources
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
       nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
         (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; List of packages to ensure are installed and active
(ensure-package-installed 'evil-mode
			  'evil-leader
			  'evil-nerd-commenter
			  'helm
			  'powerline)

;;
;; Package Settings
;;

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
