;; init-lang-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

;; NOTE: clang language server
(use-package cc-mode
  ;; By default files ending in .h are treated as c files rather than c++ files.
  :mode ("\\.h\\'" . c++-mode)
  :hook ((c-mode c++-mode) . lsp-bridge-mode)
  :config
  (add-hook-run-once 'c-mode-hook '+lsp/set-leader-keys)
  (add-hook-run-once 'c++-mode-hook '+lsp/set-leader-keys))

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(with-eval-after-load 'make-mode
  ;; Makefile don't ask me 'Suspicious line XXX. Save anyway'
  (add-hook 'makefile-mode-hook
            (function
             (lambda ()
               (fset 'makefile-warn-suspicious-lines 'ignore)))))


(provide 'init-lang-c)

;;; init-lang-c.el ends here
