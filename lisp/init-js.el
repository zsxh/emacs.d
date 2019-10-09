;; init-js.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  for javascript
;;

;;; Code:

(require 'init-language-server)

;; Install js language server
;; npm i -g typescript typescript-language-server
(dolist (mode '(js-mode js2-mode))
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
  (require 'nvm)
  (lsp)
  (+language-server/set-common-leader-keys mode-map))


(provide 'init-js)

;;; init-js.el ends here
