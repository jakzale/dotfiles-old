;; Prelude personal configuration

;; Load org mode and trello integration
(require 'org-trello)

;; Trello org-mode configuration

;; recognise *.trello files ase org-mode files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (let ((fname (buffer-file-name (current-buffer))))
              (when (and fname (string= "trello" (file-name-extension fname)))
                (org-trello-mode)))))
