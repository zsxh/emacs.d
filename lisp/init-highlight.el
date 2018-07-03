;; -*- lexical-binding: t -*-

;; Highlight matching parenthesis
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  ;; There's no fringe when Emacs is running in the console,
  ;; but the navigation and revert commands still work.
  ;; Consider turning diff-hl-margin-mode on, to show the indicators in the margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  )



(provide 'init-highlight)
