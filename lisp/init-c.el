;; init-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

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
  :defer t
  :quelpa ((ccls :fetcher github :repo "MaskRay/emacs-ccls")))

(defun +c/lsp-ccls-config ()
  (require 'ccls)
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (+c/set-leader-keys)
  (lsp)
  ;; (setq-local company-backends
  ;;             '((company-lsp :separate company-yasnippet)))
  )

(add-hook 'c-mode-hook '+c/lsp-ccls-config)
(add-hook 'c++-mode-hook '+c/lsp-ccls-config)

(use-package cmake-mode
  :ensure t)


(provide 'init-c)

;;; init-c.el ends here
