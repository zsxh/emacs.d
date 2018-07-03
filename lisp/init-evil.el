;; -*- lexical-binding: t -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'dired
    (evil-collection-init 'dired))
  (with-eval-after-load 'ibuffer
    (evil-collection-init 'ibuffer))
  (with-eval-after-load 'ediff
    (evil-collection-init 'ediff))
  (with-eval-after-load 'flycheck
    (evil-collection-init 'flycheck))
  )


(provide 'init-evil)
