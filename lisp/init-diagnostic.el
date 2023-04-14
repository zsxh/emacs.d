;; init-diagnostic.el --- Diagnostic Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Diagnostic Configuations
;;

;;; Code:

;;;;;;;;;;;;;; FLYMAKE ;;;;;;;;;;;;;;

(use-package flymake
  :defer t
  :ensure nil
  :config
  (setq flymake-no-changes-timeout 2
        flymake-start-on-save-buffer nil
        flymake-show-diagnostics-at-end-of-line t)
  (advice-add 'flymake-eldoc-function :override #'ignore))

;; TODO: flymake-collection
;; https://github.com/mohkale/flymake-collection


(provide 'init-diagnostic)

;;; init-diagnostic.el ends here
