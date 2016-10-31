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

;; Set up Scheme Mode
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

;; Set up Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Load proof general
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Set up tuareg mode
(autoload 'ocamldebug "ocamldebug" "\
Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for ocamldebug.  If you wish to change this, use
the ocamldebug commands `cd DIR' and `directory'.

\(fn PATH)" t nil)

(defalias 'camldebug 'ocamldebug)

;;;***

;;;### (autoloads nil "tuareg" "tuareg.el" (22544 40073 167174 628000))
;;; Generated autoloads from tuareg.el
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
               ".annot" ".cmt" ".cmti"))
 (add-to-list 'completion-ignored-extensions ext))

(autoload 'tuareg-mode "tuareg" "\
Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is
included.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}

\(fn)" t nil)

(autoload 'tuareg-run-ocaml "tuareg" "\
Run an OCaml toplevel process.  I/O via buffer `*ocaml-toplevel*'.

\(fn)" t nil)

(defalias 'run-ocaml 'tuareg-run-ocaml)

(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))

(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))



;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)

;; Use IDO mode
(require 'ido)
(ido-mode t)

;; Require Org-markdown support by default
(require 'ox-md)

;; My attempt at writing a plugin
;; Load the string library
(require 'subr-x)
(format-time-string "%y-%m-%d")

(defvar university-log-directory
  "~/src/university/research-log"
  "The directory for the university log")

;; Add the university log to agenda files
(add-to-list 'org-agenda-files university-log-directory)

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
  (find-file (format "%s/%s" university-log-directory f)))

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


;; Better Org Exporting
(defun org-export-all (backend)
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Note that subtrees must have the :EXPORT_FILE_NAME: property set
to a unique value for this to work properly."
  (interactive "sEnter backend: ")
  (let ((fn (cond ((equal backend "html") 'org-html-export-to-html)
                  ((equal backend "latex") 'org-latex-export-to-latex)
                  ((equal backend "pdf") 'org-latex-export-to-pdf)
		  ((equal backend "ascii") 'org-ascii-export-to-ascii))))
    (save-excursion
      (set-mark (point-min))
      (goto-char (point-max))
      (org-map-entries (lambda () (funcall fn nil t)) "-noexport" 'region-start-level))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" default)))
 '(org-latex-pdf-process (quote ("latexmk -xelatex %f")))
 '(package-selected-packages
   (quote
    (js2-mode solarized-theme markdown-mode auctex git-gutter rainbow-delimiters paredit magit intero org zenburn-theme geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
