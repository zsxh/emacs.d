;; -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t))

(provide 'init-yasnippet)
