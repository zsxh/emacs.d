;; init-js.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  for javascript
;;

;;; Code:

(require 'init-language-server)

;; npm install -g vue-language-server
(use-package vue-mode
  :ensure t
  :commands vue-mode
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
    (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))

  (with-eval-after-load 'js
    (+funcs/major-mode-leader-keys
     js-mode-map
     "'" '(vue-mode-edit-indirect-at-point :which-key "vue-edit-block"))))

(use-package rjsx-mode
  :ensure t
  :mode ("components\\/.*\\.js\\'" . rjsx-mode)
  :commands rjsx-mode)

;; Install js language server
;; npm i -g typescript typescript-language-server
(dolist (mode '(js-mode js2-mode vue-mode rjsx-mode))
  (let ((mode-hook (intern (format "%s-hook" mode)))
        (mode-map (intern (format "%s-map" mode))))
    (add-hook mode-hook (lambda () (+js/config mode-map)))))

(use-package nvm
  :ensure t
  :defer t
  :config
  ;; Lazy load node/npm/nvm in my zsh enviroment configuration (cause nvm startup very slow),
  ;; we need to explicitly set nvm enviroment in emacs, otherwise lsp will not able to find the client command.
  (nvm-use (string-trim (shell-command-to-string "nvm current"))))

(with-eval-after-load 'js
  (setq js-indent-level 2))

(with-eval-after-load 'js2-mode
  (setq js2-basic-offset 2))

(defun +js/config (mode-map)
  (unless (eq major-mode 'json-mode)
    (require 'nvm)
    (lsp)
    (+language-server/set-common-leader-keys mode-map)))


(provide 'init-js)

;;; init-js.el ends here
