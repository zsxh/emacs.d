;; init-lang-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

;; NOTE: clang language server,
;; https://clangd.llvm.org/installation#project-setup
;; https://clang.llvm.org/docs/ClangFormatStyleOptions.html

(when (treesit-ready-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-hook-run-once 'c-mode-hook '+eglot/set-leader-keys)
  (add-hook-run-once 'c++-mode-hook '+eglot/set-leader-keys))

(use-package c-ts-mode
  :ensure nil
  :hook (c-ts-base-mode . eglot-ensure)
  :config
  (add-hook-run-once 'c-ts-mode-hook '+eglot/set-leader-keys)
  (add-hook-run-once 'c++-ts-mode-hook '+eglot/set-leader-keys))

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
