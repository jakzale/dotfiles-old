;; Really Basic Emacs file :p

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

(package-initialize)


;; Disable the slash screen
(setq inhibit-splash-screen t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" default)))
 '(package-selected-packages (quote (zenburn-theme geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
