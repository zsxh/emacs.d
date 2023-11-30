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
  ;; NOTE: eglot does not respect `flymake-no-changes-timeout' option right now
  ;; New LSP versions support so-called "pull diagnostics", but not all servers support this necessarily.
  ;; only actually and eagerly report LSP diagnotics if the user has Flymake starting automatically on a timer (`flymake-no-changes-timeout' is a number).
  ;; https://github.com/joaotavora/eglot/discussions/1083
  ;; https://github.com/joaotavora/eglot/discussions/964
  ;; https://github.com/joaotavora/eglot/commit/2b87b06d9ef15e7c39d87fd5a4375b6deaa7e322
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_pullDiagnostics
  (setq flymake-no-changes-timeout 0.5
        flymake-start-on-save-buffer nil
        ;; FIXME: mess up corfu popup child frame
        flymake-show-diagnostics-at-end-of-line nil)
  (advice-add 'flymake-eldoc-function :override #'ignore))

;; https://github.com/mohkale/flymake-collection
;; TODO: flymake checkers for non-lsp files: yaml, xml, json...
(use-package flymake-collection
  :defer t)


(provide 'init-diagnostic)

;;; init-diagnostic.el ends here
