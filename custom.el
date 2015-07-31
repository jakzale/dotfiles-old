;;;; package --- My own custom configuration
;;;; Commentary:
;;;;   My own custom configuration

;;;; Code:

;; First of all -- defining proper emacs keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Structured Haskell Mode configuration

;; Trying not to pollute the global space
(let ((my-hindent-path (expand-file-name "~/src/hindent/elisp"))
      (my-shm-path (expand-file-name "~/src/structured_haskell_mode/elisp")))
  (add-to-list 'load-path my-hindent-path)
  (byte-recompile-directory my-hindent-path 0)
  (add-to-list 'load-path my-shm-path)
  (byte-recompile-directory my-shm-path 0))

(require 'hindent)
(require 'shm)

(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

;; Load my own plugins
(require 'org-trello)
(require 'pomodoro)

;; Add pomodoro to modeline
(pomodoro-add-to-mode-line)

;; Trello org-mode configuration

;; recognise *.trello files ase org-mode files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (let ((fname (buffer-file-name (current-buffer))))
              (when (and fname (string= "trello" (file-name-extension fname)))
                (org-trello-mode)))))

;;;; Custom set variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-journal-dir "~/Dropbox/journal/")
 '(org-trello-current-prefix-keybinding "C-c x" nil (org-trello))
 '(pomodoro-play-sounds nil)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
