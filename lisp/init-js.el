;; init-js.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  for javascript
;;

;;; Code:

(require 'init-language-server)

(defun +js/lsp-js-config ()
  (lsp))

(add-hook 'js-mode-hook '+js/lsp-js-config)
(add-hook 'typescript-mode-hook '+js/lsp-js-config)
(add-hook 'js3-mode-hook '+js/lsp-js-config)
(add-hook 'rjsx-mode-hook '+js/lsp-js-config)

(provide 'init-js)

;;; init-js.el ends here
