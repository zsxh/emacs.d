;; init-lang-js.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  for javascript
;;

;;; Code:

;; NOTE: JavaScript Package Manager: pnpm
;; https://pnpm.io

;; NOTE: Build tool: Vite
;; https://github.com/vitejs/vite

;; NOTE: Install TypeScript & JavaScript Language Server `typescript-language-server'

(when (treesit-ready-p 'javascript)
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))

(use-package js
  :ensure nil
  :bind ((:map js-base-mode-map
          ("/" . sgml-slash)))
  :hook (js-base-mode . +js/lsp-setup)
  :config
  (require 'sgml-mode)
  (setq js-indent-level 2)
  (+eglot/set-leader-keys js-mode-map)
  (+eglot/set-leader-keys js-ts-mode-map)

  (defun +js/lsp-setup ()
    ;; This fix beginning-of-defun raise exception problem
    (setq-local beginning-of-defun-function #'js-beginning-of-defun)
    (eglot-ensure)))

(if (treesit-ready-p 'typescript)
    (use-package typescript-ts-mode
      :ensure nil
      :mode (("\\.ts\\'" . typescript-ts-mode)
             ("\\.tsx\\'" . tsx-ts-mode))
      :defer t
      :hook (typescript-ts-base-mode . eglot-ensure)
      :config
      (+eglot/set-leader-keys typescript-ts-mode-map)
      (+eglot/set-leader-keys tsx-ts-mode-map))
  (use-package typescript-mode
    :defer t
    ;; :init
    ;; (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
    :hook (typescript-mode . eglot-ensure)
    :config
    (+eglot/set-leader-keys typescript-mode-map)))





(provide 'init-lang-js)

;;; init-lang-js.el ends here
