;; init-lang-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

(require 'init-language-server)

;; By default files ending in .h are treated as c files rather than c++ files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package ccls
  :quelpa ((ccls :fetcher github :repo "MaskRay/emacs-ccls"))
  :defer t
  :config
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
  (+language-server/set-common-leader-keys c-mode-map)
  (+language-server/set-common-leader-keys c++-mode-map)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(add-hook-run-once 'c-mode-hook (lambda () (require 'ccls) (lsp)))
(add-hook-run-once 'c++-mode-hook (lambda () (require 'ccls) (lsp)))

(use-package cmake-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)))


(provide 'init-lang-c)

;;; init-lang-c.el ends here
