;; -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t))

(with-eval-after-load 'yasnippet
  (require 'init-which-key)
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'snippet-mode-map
   :major-modes t
   :prefix "SPC"
   "m"   '(nil :which-key "major")
   "mt"   '(yas-tryout-snippet :which-key "yas-tryout-snippet"))
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'snippet-mode-map
   :major-modes t
   :prefix ","
   "t"   '(yas-tryout-snippet :which-key "yas-tryout-snippet"))
  )

(provide 'init-yasnippet)
