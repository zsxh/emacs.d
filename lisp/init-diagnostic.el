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

;; https://github.com/mohkale/flymake-collection
;; TODO: flymake checkers for non-lsp files: yaml, xml, json...
(use-package flymake-collection
  :defer t)


(provide 'init-diagnostic)

;;; init-diagnostic.el ends here
