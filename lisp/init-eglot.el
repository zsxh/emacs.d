;; init-eglot.el --- language server protocol client eglot	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Language Server Protocol Emacs Client Eglot
;;

;;; Code:

;; NOTE: https://joaotavora.github.io/eglot/
;; NOTE: macos file descriptors limitation, https://www.reddit.com/r/emacs/comments/x4p7mg/fix_annoying_max_open_files_for_emacs/
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        ;; NOTE: drop log to improve performance
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-report-progress nil
        eglot-stay-out-of '()
        eglot-extend-to-xref t
        eglot-code-action-indications '(margin))
  (add-hook 'eglot-managed-mode-hook #'breadcrumb-local-mode)
  (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs))

(with-eval-after-load 'eglot

  (defvar +eglot/display-buf "*+eglot/display-buffer*")
  (defvar +eglot/display-frame nil)
  (defvar +eglot/hover-last-point nil)
  (defface +eglot/display-border '((((background dark)) . (:background "white"))
                                   (((background light)) . (:background "black")))
    "The border color used in childframe.")

  (cl-defgeneric +eglot/workspace-configuration (server)
    "Set workspace configuration,
- Handle server request `workspace/configuration'
- Send a `workspace/didChangeConfiguration' signal to SERVER"
    nil)
  (setq-default eglot-workspace-configuration #'+eglot/workspace-configuration)

  ;; Hover
  (defun +eglot/show-hover-at-point ()
    (interactive)
    (when (eglot-server-capable :hoverProvider)
      (let ((buf (current-buffer)))
        (jsonrpc-async-request
         (eglot--current-server-or-lose)
         :textDocument/hover (eglot--TextDocumentPositionParams)
         :success-fn (eglot--lambda ((Hover) contents range)
                       (eglot--when-buffer-window buf
                         (if-let* ((info (unless (seq-empty-p contents)
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
      (posframe-hide +eglot/display-buf))))

(defun +eglot/set-leader-keys (&optional map)
  (let ((mode-map (or map (keymap-symbol (current-local-map)))))
    (+funcs/major-mode-leader-keys
     mode-map
     ;; code action
     "A" '(eglot-code-actions :which-key "code-action")
     ;; hover
     "D" '(+eglot/show-hover-at-point :which-key "hover")

     ;; FIXME: repeat mode
     ;; debug
     ;; "d" '(nil :which-key "debug")
     ;; "dd" '(dape :which-key "dape")
     ;; "dp" '(dape-pause :which-key "dape-pause")
     ;; "dc" '(dape-continue :which-key "dape-continue")
     ;; "dn" '(dape-next :which-key "dape-next")
     ;; "ds" '(dape-step-in :which-key "dape-step-in")
     ;; "do" '(dape-step-out :which-key "dape-step-out")
     ;; "dr" '(dape-restart :which-key "dape-restart")
     ;; "di" '(dape-info :which-key "dape-info")
     ;; "dR" '(dape-repl :which-key "dape-repl")
     ;; "dm" '(dape-read-memory :which-key "dape-read-memory")
     ;; "dl" '(dape-log-breakpoint :which-key "dape-log-breakpoint")
     ;; "de" '(dape-expression-breakpoint :which-key "dape-expression-breakpoint")
     ;; "db" '(dape-toggle-breakpoint :which-key "dape-toggle-breakpoint")
     ;; "dB" '(dape-remove-all-breakpoints :which-key "dape-remove-all-breakpoints")
     ;; "dw" '(dape-watch-dwim :which-key "dape-watch-dwim")
     ;; "dq" '(dape-quit :which-key "dape-quit")

     ;; error
     "e" '(nil :which-key "error")
     "el" '(consult-flymake :which-key "show-buffer-diagnostics")
     "eL" '((lambda () (interactive) (consult-flymake t)) :which-key "show-project-diagnostics")
     "en" '(flymake-goto-next-error :which-key "next-error")
     "ep" '(flymake-goto-prev-error :which-key "prev-error")
     ;; format
     "f" '(eglot-format :which-key "format")
     ;; goto/find
     "g" '(nil :which-key "find/goto")
     "gd" '(xref-find-definitions :which-key "find-definitions")
     "ge" '(eglot-find-declaration :which-key "find-declaration")
     "gi" '(eglot-find-implementation :which-key "find-implementation")
     "gr" '(xref-find-references :which-key "find-references")
     "gt" '(eglot-find-typeDefinition :which-key "find-typeDefinition")
     "gs" '(consult-eglot-symbols :which-key "workspace-symbols")
     ;; hierarchy
     "h" '(nil :which-key "hierarchy")
     "hc" '(eglot-hierarchy-call-hierarchy :which-key "call-hierarchy")
     "ht" '(eglot-hierarchy-type-hierarchy :which-key "type-hierarchy")
     ;; rename
     "R" '(eglot-rename :which-key "rename")
     ;; signature
     "S" '(+eglot/signature-help-at-point :which-key "signatureHelp"))))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :defer t)

(use-package eglot-hierarchy
  :vc (:url "https://github.com/dolmens/eglot-hierarchy")
  :defer t)

;; https://github.com/liushihao456/symbols-outline.el
(use-package symbols-outline
  :defer t
  :config
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (with-eval-after-load 'evil
    (evil-define-key* 'normal symbols-outline-mode-map
      "j" #'symbols-outline-next
      "k" #'symbols-outline-prev
      "gr" #'symbols-outline-refresh)))

;; NOTE: Install emacs-lsp-booster from https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
	:after eglot
	:config
  (setq eglot-booster-no-remote-boost t
        eglot-booster-io-only t)
  (eglot-booster-mode))

(use-package eglot-inactive-regions
  :after eglot
  :config
  (eglot-inactive-regions-mode 1))

;; TODO: https://codeberg.org/harald/eglot-supplements


(provide 'init-eglot)

;;; init-eglot.el ends here
