;; Really Basic Emacs file :p

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

(package-initialize)

;; Load zenburn theme
(load-theme 'zenburn t)

;; Disable the slash screen
(setq inhibit-splash-screen t)

;; Set C-x O to get the previous window
(global-set-key (kbd "C-x O")
		(lambda ()
		  (interactive)
		  (other-window -1)))

;; Set C-x g to magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Add intero to haskell
(add-hook 'haskell-mode-hook 'intero-mode)

;; Load proof general
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; Use IDO mode
(require 'ido)
(ido-mode t)

;; My attempt at writing a plugin
(format-time-string "%y-%m-%d")

(defun university-sanitize-entry (s)
  "Sanitize by replacing everything non-alnum as string, then
trimming, then replacing all space by '-'."
  (downcase (replace-regexp-in-string "[[:space:]]+"
				      "-"
				      (string-trim (replace-regexp-in-string "[^[:alnum:]]+"
									     " "
									     s)))))

(defun university-find-log-entry (f)
  "Simple function to find an entry in my university log"
  (find-file (format "~/src/university/research-log/%s" f)))

(defun university-log-new ()
  "Prompt user to enter a new log entry"
  (interactive)
  ;; Get the local date
  (let ((date (format-time-string "%y-%m-%d")))
    ;; Ask for an entry name for the date
    (let ((entry (read-string (format "New entry [%s]:"
				      date))))
      (let ((entry-file (format "%s-%s.org"
				date (university-sanitize-entry entry))))
	(message "New log entry: %s"
		 (university-find-log-entry entry-file))))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" default)))
 '(org-latex-pdf-process (quote ("latexmk -xelatex %f")))
 '(package-selected-packages
   (quote
    (rainbow-delimiters paredit magit intero org zenburn-theme geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
