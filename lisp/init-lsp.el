;; init-lsp.el --- lsp	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  lsp
;;

;;; Code:

;; TODO: `lsp-bridge'
(use-package lsp-bridge
  :load-path "~/.emacs.d/submodules/lsp-bridge"
  :commands (lsp-bridge-mode)
  :config
  (setq
   lsp-bridge-enable-debug nil
   lsp-bridge-enable-log t
   lsp-bridge-python-command (expand-file-name "~/.pyenv/versions/3.10.5/bin/python")
   lsp-bridge-get-project-path-by-filepath
   (lambda (filepath)
     "Customize lsp-bridge get project root function"
     (when-let ((root (+project/root nil (directory-file-name filepath))))
       (expand-file-name root))))
  (require 'lsp-bridge-jdtls))

(use-package lsp-bridge-jdtls
  :ensure nil
  :defer t
  :config
  (setq lsp-bridge-jdtls-default-file
        (expand-file-name "lsp-bridge-config/jdtls.json" user-emacs-directory)
        lsp-bridge-jdtls-jvm-args
        `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar")))))

(add-hook 'java-mode-hook 'lsp-bridge-mode)




(provide 'init-lsp)

;;; init-lsp.el ends here
