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
  (setq flymake-no-changes-timeout nil
        flymake-start-on-save-buffer nil
        ;; FIXME: mess up corfu popup child frame, https://github.com/minad/corfu/issues/414
        flymake-show-diagnostics-at-end-of-line 'short)

  (advice-add 'flymake-eldoc-function :override #'ignore)

  ;; NOTE: https://github.com/joaotavora/eglot/issues/1296#issuecomment-1727978307
  (with-eval-after-load 'eglot
    (cl-defmethod eglot-handle-notification :after
      (_server (_method (eql textDocument/publishDiagnostics)) &key uri
               &allow-other-keys)
      (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
        (with-current-buffer buffer
          (if (and (eq nil flymake-no-changes-timeout)
                   (not (buffer-modified-p)))
              (flymake-start t)))))))

;; https://github.com/mohkale/flymake-collection
;; TODO: flymake checkers for non-lsp files: yaml, xml, json...
(use-package flymake-collection
  :defer t)


(provide 'init-diagnostic)

;;; init-diagnostic.el ends here
