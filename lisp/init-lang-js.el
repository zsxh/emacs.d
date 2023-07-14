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

;; NOTE: Install TypeScript & JavaScript Language Server
;; https://github.com/typescript-language-server/typescript-language-server
;; pnpm add -g typescript-language-server typescript

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
    (unless (member major-mode '(js-json-mode ein:ipynb-mode))
      (eglot-ensure)))

  (+funcs/major-mode-leader-keys js-json-mode-map
                                 "A" nil
                                 "d" nil
                                 "D" nil
                                 "e" nil
                                 "f" nil
                                 "g" nil
                                 "l" nil
                                 "p" '(json-pretty-print-buffer :which-key "pretty-print")
                                 "R" nil))

(use-package json-ts-mode
  :ensure nil
  :defer t
  :config
  (+funcs/major-mode-leader-keys json-ts-mode-map
                                 "j" '(counsel-jq :which-key "counsel-jq")
                                 "p" '(json-pretty-print-buffer :which-key "pretty-print")))

(use-package typescript-mode
  :defer t
  ;; :init
  ;; (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
  :hook (typescript-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys typescript-mode-map))

(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :hook (typescript-ts-base-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys typescript-ts-mode-map)
  (+eglot/set-leader-keys tsx-ts-mode-map))

;; NOTE: vue-language-server
(use-package vue-mode
  :commands vue-mode
  :hook (vue-mode . eglot-ensure)
  :bind ((:map vue-mode-map
               ("C-c C-l" . vue-mode-reparse)))
  :config
  ;; NOTE: lsp-vetur (lsp-format-buffer) just ignore .eslintrc.js file
  ;; so we need to set .eslintrc.js configs to avoid eslint error, check https://eslint.org/ for details
  ;; rules: {
  ;;   'semi': ['error', 'always'],
  ;;   'quotes': [2, 'double', { 'avoidEscape': true }]
  ;; }
  (defun eslint/binary ()
    (or
     ;; Try to find bin in node_modules (via 'npm install prettier-eslint-cli')
     (let ((root (locate-dominating-file buffer-file-name "node_modules")))
       (if root
           (let ((prettier-binary (concat root "node_modules/.bin/eslint")))
             (if (file-executable-p prettier-binary) prettier-binary))))
     ;; Fall back to a globally installed binary
     (executable-find "eslint")
     ;; give up
     (error "Couldn't find a eslint executable")))

  (defun prettier-eslint ()
    "Format the current file with ESLint."
    (interactive)
    (progn (call-process (eslint/binary)
                         nil "*Prettier-ESLint Errors*" nil
                         buffer-file-name "--fix")
           (revert-buffer t t t)))

  (with-eval-after-load 'mmm-mode
    ;; the indentation in the <script> tag is broken, with new lines aligned on the left.
    ;; https://github.com/AdamNiederer/vue-mode/issues/74
    (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
    (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))))


(provide 'init-lang-js)

;;; init-lang-js.el ends here
