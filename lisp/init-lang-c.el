;; init-lang-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

(require 'init-language-server)

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode) ;; By default files ending in .h are treated as c files rather than c++ files.
  :config
  (add-hook-run-once 'c-mode-hook '+c/setup)
  (add-hook-run-once 'c++-mode-hook '+c/setup))

(use-package ccls
  :quelpa ((ccls :fetcher github :repo "MaskRay/emacs-ccls"))
  :defer t
  :config
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

(defun +c/setup ()
  (require 'ccls)
  (let ((mode-map (symbol-value (intern (format "%s-map" major-mode))))
        (mode-hook (intern (format "%s-hook" major-mode))))
    (+language-server/set-common-leader-keys mode-map)
    (add-hook mode-hook 'lsp))
  (lsp))

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


(provide 'init-lang-c)

;;; init-lang-c.el ends here
