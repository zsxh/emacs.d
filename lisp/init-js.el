;; init-js.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  for javascript
;;

;;; Code:

(require 'init-language-server)

(use-package nvm
  :ensure t
  :after js)

;; Install js language server
;; npm i -g typescript typescript-language-server
(use-package js
  :ensure nil
  :preface
  (defun +js/lsp ()
    ;; This fix beginning-of-defun raise exception problem
    (setq-local beginning-of-defun-function #'js-beginning-of-defun)
    (unless (member major-mode '(json-mode ein:ipynb-mode))
      (lsp)))
  :hook (js-mode . +js/lsp)
  :bind ((:map js-mode-map ("/" . sgml-slash)))
  :config
  (require 'sgml-mode)
  (setq js-indent-level 2)
  (+language-server/set-common-leader-keys js-mode-map))

;; npm install -g vue-language-server
(use-package vue-mode
  :ensure t
  :commands vue-mode
  :hook (vue-mode . lsp)
  :bind ((:map vue-mode-map
               ("C-c C-l" . vue-mode-reparse)))
  :config
  ;; FIXME: lsp-vetur (lsp-format-buffer) just ignore .eslintrc.js file
  ;; so we need to set .eslintrc.js configs to avoid eslint error, check https://eslint.org/ for details
  ;; rules: {
  ;;   'semi': ['error', 'always'],
  ;;   'quotes': [2, 'double', { 'avoidEscape': true }]
  ;; }
  (with-eval-after-load 'lsp-vetur
    (setq lsp-vetur-format-default-formatter-js "prettier-eslint"))

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


(provide 'init-js)

;;; init-js.el ends here
