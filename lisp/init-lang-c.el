;; init-lang-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

(require 'init-lsp)

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode) ;; By default files ending in .h are treated as c files rather than c++ files.
  :config
  (add-hook-run-once 'c-mode-hook '+c/setup)
  (add-hook-run-once 'c++-mode-hook '+c/setup))

(use-package ccls
  :defer t
  :config
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

;; https://emacs-lsp.github.io/dap-mode/page/configuration/#native-debug-gdblldb
;; run `dap-gdb-lldb-setup' to setup automatically(auto download)
(use-package dap-gdb-lldb
  :after ccls
  :ensure dap-mode)

;; https://emacs-lsp.github.io/dap-mode/page/configuration/#vscode-cpptools
;; run `dap-cpptools-setup' to setup automatically(auto download)
(use-package dap-cpptools
  :after ccls
  :ensure dap-mode
  :config
  ;; TODO: auto build before launching debugger
  (dap-register-debug-template
   "cpptools::Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run Configuration"
         :MIMode "gdb"
         :program "${workspaceFolder}/${fileBasenameNoExtension}"
         :cwd "${workspaceFolder}")))

(defun +c/setup ()
  (require 'ccls)
  ;; (add-hook 'lsp-after-initialize-hook
  ;;           (lambda ()
  ;;             (when (member major-mode '(c-mode c++-mode))
  ;;               (ccls-code-lens-mode)
  ;;               (make-variable-buffer-local 'hs-hide-hook)
  ;;               (add-hook 'hs-hide-hook 'ccls-clear-code-lens)
  ;;               (make-variable-buffer-local 'hs-show-hook)
  ;;               (add-hook 'hs-show-hook 'ccls-request-code-lens))))
  (let ((mode-map-symbol (intern (format "%s-map" major-mode)))
        (mode-hook (intern (format "%s-hook" major-mode))))
    (+language-server/set-common-leader-keys mode-map-symbol)
    (add-hook mode-hook 'lsp-deferred))
  (lsp-deferred))

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
