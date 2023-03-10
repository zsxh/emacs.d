;; init-eglot.el --- language server protocol client eglot	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Language Server Protocol Emacs Client Eglot
;;

;;; Code:

(when (bound-and-true-p read-process-output-max)
  (setq read-process-output-max (* 1024 1024)))

;; NOTE: https://joaotavora.github.io/eglot/
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        ;; NOTE: drop jsonrpc log to improve performance
        eglot-events-buffer-size 0
        eglot-report-progress nil)

  (defvar eglot-path-uri-cache (make-hash-table :test #'equal)
    "Cache file path to uri relations.")

  (cl-defgeneric +eglot/ext-uri-to-path (uri)
    "Support extension uri."
    nil)

  (define-advice eglot--uri-to-path (:around (orig-fn uri) advice)
    "Support non standard LSP uri scheme."
    (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
    (or (+eglot/ext-uri-to-path uri)
        (funcall orig-fn uri)))

  (define-advice eglot--path-to-uri (:around (orig-fn path) advice)
    "Support non standard LSP uri scheme."
    (or (gethash path eglot-path-uri-cache)
        (funcall orig-fn path))))

(defun +eglot/set-leader-keys (&optional map)
  (let ((mode-map (or map (keymap-symbol (current-local-map)))))
    (+funcs/major-mode-leader-keys
     mode-map
     "A" '(eglot-code-actions :which-key "code-action")
     "D" '(eldoc-box-eglot-help-at-point :which-key "hover:document")
     "e" '(nil :which-key "error")
     "el" '(consult-flymake :which-key "show-buffer-diagnostics")
     "eL" '((lambda () (interactive) (consult-flymake t)) :which-key "show-project-diagnostics")
     "en" '(flymake-goto-next-error :which-key "next-error")
     "ep" '(flymake-goto-prev-error :which-key "prev-error")
     "f" '(eglot-format :which-key "format")
     "g" '(nil :which-key "find/goto")
     "gd" '(xref-find-definitions :which-key "find-definitions")
     "ge" '(eglot-find-declaration :which-key "find-declaration")
     "gi" '(eglot-find-implementation :which-key "find-implementation")
     "gr" '(xref-find-references :which-key "find-references")
     "gt" '(eglot-find-typeDefinition :which-key "find-typeDefinition")
     "gs" '(consult-eglot-symbols :which-key "workspace-symbols")
     "R" '(eglot-rename :which-key "rename"))))

(use-package eldoc
  :ensure nil
  :defer t
  :config
  (setq eldoc-echo-area-use-multiline-p 1))

(use-package eldoc-box
  :commands (eldoc-box-eglot-help-at-point))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :defer t)


(provide 'init-eglot)

;;; init-eglot.el ends here
