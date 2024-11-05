;; init-lang-lua.el --- Lua Configs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Lua Configs
;;

;;; Code:

(use-package lua-ts-mode
  :hook (lua-ts-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys lua-ts-mode-map))


(provide 'init-lang-lua)

;;; init-lang-lua.el ends here
