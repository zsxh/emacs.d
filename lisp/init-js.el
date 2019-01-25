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
(dolist (js-lang '(js typescript js3 rjsx))
  (let ((js-lang-hook (intern (format "%s-mode-hook" js-lang))))
    (add-hook js-lang-hook 'lsp)))


(provide 'init-js)

;;; init-js.el ends here
