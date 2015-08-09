;;;; package --- My own custom configuration
;;;; Commentary:
;;;;   My own custom configuration

;;;; Code:

;; First of all -- defining proper emacs keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Structured Haskell Mode configuration

;; ;; Trying not to pollute the global space
;; (let ((my-hindent-path (expand-file-name "~/src/hindent/elisp"))
;;       (my-shm-path (expand-file-name "~/src/structured_haskell_mode/elisp")))
;;   (add-to-list 'load-path my-hindent-path)
;;   (byte-recompile-directory my-hindent-path 0)
;;   (add-to-list 'load-path my-shm-path)
;;   (byte-recompile-directory my-shm-path 0))

;; (require 'prelude-programming)
;; (prelude-require-packages '(haskell-mode))

;; (require 'hindent)
;; (require 'shm)

;; ;; Slightly modified prelude-haskell

;; (defun my-nonclashing-haskell-mode-keys ()
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;;   ;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
;;   )

;; (defun my-nonclashing-cabal-mode-keys ()
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))

;; ;; Also, manually setting up bindings for cabal
;; (eval-after-load 'haskell-cabal
;;   '(my-nonclashing-cabal-mode-keys))

;; (eval-after-load 'haskell-mode
;;   '(progn
;;      ;; Defining keys manually
;;      (my-nonclashing-haskell-mode-keys)
;;      ;; Defining custom prelude hook
;;      (defun prelude-haskell-mode-defaults ()
;;        (structured-haskell-mode +1)
;;        (hindent-mode +1))
;;      (setq prelude-haskell-mode-hook 'prelude-haskell-mode-defaults)

;;      (add-hook 'haskell-mode-hook (lambda ()
;;                                     (run-hooks 'prelude-haskell-mode-hook)))))


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

;; My custom theme swapper... Couldn't be bored to swap it manually
;; load solarized-light theme, so it can be enabled
;; Load, and do not enable
(load-theme 'solarized-light t t)

(defun my-swap-theme ()
  "A simple function to swap current theme with solarized-light or vice-versa."
  (flet ((is-light-theme ()
                         (eq 'solarized-light
                             (car custom-enabled-themes))))
    (if (is-light-theme)
        (disable-theme 'solarized-light)
      (enable-theme 'solarized-light))))

;; I will try to bind it to s-\ for now
(global-set-key (kbd "s-\\") (lambda ()
                               (interactive)
                               (my-swap-theme)))


;; Add done command to eshell
(require 'alert)

(defun eshell/done ()
  "A simple function that will notify when done"
  (alert "Done!" :persistent t))


;;;; Custom set variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style (quote notifier))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(hindent-style "johan-tibell")
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
