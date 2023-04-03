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
        eglot-send-changes-idle-time 0.3
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        ;; NOTE: drop jsonrpc log to improve performance
        eglot-events-buffer-size 0
        eglot-report-progress nil
        eglot-stay-out-of '(eldoc)))

(with-eval-after-load 'eglot
  (defvar +eglot/display-buf "*+eglot/display-buffer*")
  (defvar +eglot/display-frame nil)
  (defvar +eglot/hover-last-point nil)
  (defvar +eglot/signature-last-point nil)
  (defface +eglot/display-border '((((background dark)) . (:background "white"))
                                   (((background light)) . (:background "black")))
    "The border color used in childframe.")

  ;; Uri <-> File Path
  (defvar eglot-path-uri-cache (make-hash-table :test #'equal)
    "File path to uri cache.")

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
        (funcall orig-fn path)))

  ;; Hover
  (defun +eglot/show-hover-at-point ()
    (interactive)
    (when (eglot--server-capable :hoverProvider)
      (let ((buf (current-buffer)))
        (jsonrpc-async-request
         (eglot--current-server-or-lose)
         :textDocument/hover (eglot--TextDocumentPositionParams)
         :success-fn (eglot--lambda ((Hover) contents range)
                       (eglot--when-buffer-window buf
                         (if-let ((info (unless (seq-empty-p contents)
                                          (eglot--hover-info contents range))))
                             (progn
                               (with-current-buffer (get-buffer-create +eglot/display-buf)
                                 (erase-buffer)
                                 (insert info))
                               (setq +eglot/display-frame
                                     (posframe-show
                                      (get-buffer-create +eglot/display-buf)
                                      :border-width 1
                                      :border-color (face-background '+eglot/display-border nil t)
                                      :max-height (/ (frame-height) 3)
                                      :max-width (/ (frame-width) 3)
                                      :poshandler 'posframe-poshandler-point-bottom-left-corner-upward))
                               (run-with-timer 0.1 nil #'+eglot/hide-hover))
                           (message "LSP No Hover Document"))))
         :deferred :textDocument/hover))
      (setq +eglot/hover-last-point (point))))

  (defun +eglot/hide-hover ()
    (if (or (eq (point) +eglot/hover-last-point)
            (eq (selected-frame) +eglot/display-frame))
        (run-with-timer 0.1 nil #'+eglot/hide-hover)
      (posframe-hide +eglot/display-buf)))

  ;; Signature
  (defun +eglot/show-signature-help ()
    (when (and (eglot-managed-p)
               (eglot--server-capable :signatureHelpProvider))
      (unless +eglot/signature-last-point
        (setq +eglot/signature-last-point (point)))
      (let ((buf (current-buffer)))
        (if (eql (line-number-at-pos +eglot/signature-last-point)
                 (line-number-at-pos (point)))
            (jsonrpc-async-request
             (eglot--current-server-or-lose)
             :textDocument/signatureHelp (eglot--TextDocumentPositionParams)
             :success-fn (eglot--lambda ((SignatureHelp)
                                         signatures activeSignature activeParameter)
                           (eglot--when-buffer-window buf
                             (let ((active-sig (and (cl-plusp (length signatures))
                                                    (aref signatures (or activeSignature 0)))))
                               (if (not active-sig)
                                   (+eglot/hide-signature buf)
                                 (with-current-buffer (get-buffer-create +eglot/display-buf)
                                   (erase-buffer)
                                   (insert (or (eglot--sig-info active-sig t activeParameter) "")))
                                 (setq +eglot/display-frame
                                       (posframe-show
                                        (get-buffer-create +eglot/display-buf)
                                        :position +eglot/signature-last-point
                                        :border-width 1
                                        :border-color (face-background '+eglot/display-border nil t)
                                        :max-width (/ (frame-width) 3)
                                        :poshandler 'posframe-poshandler-point-bottom-left-corner-upward))))))
             :timeout-fn (lambda () (+eglot/hide-signature buf))
             :error-fn (lambda () (+eglot/hide-signature buf))
             :deferred :textDocument/signatureHelp)
          (+eglot/hide-signature buf)))))

  (defun +eglot/hide-signature (buf)
    (with-current-buffer buf
      (remove-hook 'post-command-hook #'+eglot/show-signature-help t))
    (setq +eglot/signature-last-point nil
          +eglot/signature-retries 0)
    (posframe-hide +eglot/display-buf))

  (defun +eglot/signature-help-at-point ()
    (interactive)
    (when (and (eglot-managed-p)
               (eglot--server-capable :signatureHelpProvider))
      (+eglot/show-signature-help)
      (add-hook 'post-command-hook #'+eglot/show-signature-help nil t)))

  (with-eval-after-load 'company
    (define-advice company-complete-selection (:after (&rest _) eglot-signature)
      (+eglot/signature-help-at-point))))

(defun +eglot/set-leader-keys (&optional map)
  (let ((mode-map (or map (keymap-symbol (current-local-map)))))
    (+funcs/major-mode-leader-keys
     mode-map
     "A" '(eglot-code-actions :which-key "code-action")
     "D" '(+eglot/show-hover-at-point :which-key "hover")
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
     "R" '(eglot-rename :which-key "rename")
     "S" '(+eglot/signature-help-at-point :which-key "signatureHelp"))))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :defer t)


(provide 'init-eglot)

;;; init-eglot.el ends here
