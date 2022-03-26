;; init-syntax-checking.el --- Syntax Checking Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Syntax Checking Configuations
;;

;;; Code:

;;;;;;;;;;;;;; FLYMAKE ;;;;;;;;;;;;;;

(use-package flymake
  :defer t
  :ensure nil
  :config
  (setq flymake-no-changes-timeout 2
        flymake-start-on-save-buffer nil))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))


(provide 'init-syntax-checking)

;;; init-syntax-checking.el ends here
