;; init-eglot.el --- language server protocol client eglot	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Language Server Protocol Emacs Client Eglot
;;

;;; Code:

;; TODO: lsp signature help
;; https://www.kimi.com/chat/19b5fb06-f952-8562-8000-09a662fde627
;; https://deepwiki.com/search/vscode-signature_6e97ec33-5757-48d5-876f-bf34fb2769f7?mode=fast
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_signatureHelp

;; TODO: https://github.com/sqls-server/sqls

;; NOTE: https://joaotavora.github.io/eglot/
;; NOTE: macos file descriptors limitation, https://www.reddit.com/r/emacs/comments/x4p7mg/fix_annoying_max_open_files_for_emacs/
;; NOTE: https://github.com/microsoft/vscode-extension-samples
;; NOTE: [Extending Eglot](https://elpa.gnu.org/devel/doc/eglot.html#Extending-Eglot-1)
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        eglot-stay-out-of '(eldoc)
        ;; NOTE: drop log to improve performance
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-report-progress nil
        eglot-extend-to-xref t
        ;; https://github.com/joaotavora/eglot/issues/1491
        ;; https://github.com/microsoft/vscode-extension-samples/blob/main/code-actions-sample/README.md
        eglot-code-action-indications '()
        ;; eglot-code-action-indicator ""
        eglot-advertise-cancellation nil)
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
                                      :max-width (/ (frame-width) 2)
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

  ;; [Expose textDocument/selectionRange as an interactive command #1220](https://github.com/joaotavora/eglot/discussions/1220#discussioncomment-9321061)
  ;; (global-set-key (kbd "C-=") 'eglot-expand-selection)
  (defun eglot-expand-selection ()
    "Expand the current selection to the enclosing unit of syntax,
 as furnished by an LSP `textDocument/selectionRange' request."
    ;; TODO(adonovan): add corresponding unexpand.
    (interactive)
    (let* ((resp (eglot--request (eglot--current-server-or-lose)
			                     :textDocument/selectionRange
			                     `(:textDocument ,(eglot--TextDocumentIdentifier) :positions [,(eglot--pos-to-lsp-position)])))
	       (selection-range (elt resp 0)) ; LSP SelectionRange
	       (current-range (cons (point) (if (use-region-p) (mark) (point))))
	       (new-range (eglot-range-region (plist-get selection-range :range))))
      ;; Walk down the linked list until we find an enclosing element.
      ;; We define enclosing as a proper superinterval.
      (while (let ((new-start (car new-range))
		           (new-end (cdr new-range))
		           (cur-start (car current-range))
		           (cur-end (cdr current-range)))
	           (not (and (<= new-start cur-start)
		                 (>= new-end cur-end)
		                 (> (- new-end new-start) (- cur-end cur-start)))))
        (setq selection-range (plist-get selection-range :parent))
        (setq new-range (eglot-range-region (plist-get selection-range :range))))
      ;; Finally set the selection.
      (goto-char (car new-range))
      (set-mark (cdr new-range))
      (activate-mark))))

;; https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :defer t)

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

(use-package eglot-codelens
  ;; :load-path "~/workspace/emacs/eglot-codelens"
  :vc (:url "https://github.com/zsxh/eglot-codelens"
       :rev :newest)
  :hook (eglot-managed-mode . eglot-codelens-mode))

(use-package eglot-signature
  ;; :load-path "~/workspace/emacs/eglot-signature"
  :vc (:url "https://github.com/zsxh/eglot-signature"
       :rev :newest)
  :hook (eglot-managed-mode . eglot-signature-mode)
  :init
  (with-eval-after-load 'eglot
    (eglot-signature-setup))
  :config
  (with-eval-after-load 'cape
    (advice-add 'eglot-signature--capf-wrapper :around #'cape-wrap-buster))
  (define-key eglot-signature-mode-map (kbd "S-s-SPC") #'eglot-signature-show)
  (with-eval-after-load 'evil
    (evil-make-intercept-map eglot-signature-popup-map)))

;; json/yaml/toml files metadata for lsp servers.
(defvar schemastore-url "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/api/json/catalog.json")
(defvar schemastore-file "~/.emacs.d/cache/lsp-servers/schemastore/catalog.json")
(defun +eglot/fetch-json-schema ()
  "Fetch json schema from \"https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/api/json/catalog.json\"."
  (interactive)
  (let* ((url schemastore-url)
         (buffer (plz 'get url :as 'buffer))
         (file-path schemastore-file))
    (let ((dir (file-name-directory file-path)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (with-temp-file file-path
      (erase-buffer)
      (insert-buffer-substring buffer))
    (message "Successfully fetched schemastore json schema to %s" file-path)))

;; mason.el is installer for LSP servers, DAP servers, linters and formatters
;; `mason-install', `mason-manager'
(use-package mason
  :defer t
  :config
  (setq mason-dir (expand-file-name "cache/mason" user-emacs-directory))
  (mason-ensure))

(defun +eglot/set-leader-keys (&optional map)
  (let ((mode-map (or map (keymap-symbol (current-local-map)))))
    (+funcs/major-mode-leader-keys
     mode-map
     ;; code action
     "A" '(eglot-code-actions :which-key "code-action")
     ;; hover
     "D" '(+eglot/show-hover-at-point :which-key "hover")

     ;; debug
     "d" '(nil :which-key "debug")
     "dd" '(dape :which-key "dape")
     "dp" '(dape-pause :which-key "dape-pause")
     "dc" '(dape-continue :which-key "dape-continue")
     "dn" '(dape-next :which-key "dape-next")
     "ds" '(dape-step-in :which-key "dape-step-in")
     "do" '(dape-step-out :which-key "dape-step-out")
     "dr" '(dape-restart :which-key "dape-restart")
     "df" '(dape-restart-frame :which-key "dape-restart-frame")
     "du" '(dape-until :which-key "dape-until")
     "di" '(dape-info :which-key "dape-info")
     "dR" '(dape-repl :which-key "dape-repl")
     "dm" '(dape-memory :which-key "dape-memory")
     "dM" '(dape-disassemble :which-key "dape-disassemble")
     "dl" '(dape-breakpoint-log :which-key "dape-breakpoint-log")
     "de" '(dape-breakpoint-expression :which-key "dape-breakpoint-expression")
     "dh" '(dape-breakpoint-hits :which-key "dape-breakpoint-hits")
     "db" '(dape-breakpoint-toggle :which-key "dape-breakpoint-toggle")
     "dB" '(dape-breakpoint-remove-all :which-key "dape-breakpoint-remove-all")
     "dt" '(dape-select-thread :which-key "dape-select-thread")
     "dS" '(dape-select-stack :which-key "dape-select-stack")
     "d>" '(dape-stack-select-down :which-key "dape-stack-select-down")
     "d<" '(dape-stack-select-up :which-key "dape-stack-select-up")
     "dx" '(dape-evaluate-expression :which-key "dape-evaluate-expression")
     "dw" '(dape-watch-dwim :which-key "dape-watch-dwim")
     "dD" '(dape-disconnect-quit :which-key "dape-disconnect-quit")
     "dq" '(dape-quit :which-key "dape-quit")

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
     ;; "gs" '(xref-find-apropos :which-key "workspace-symbols")
     ;; hierarchy
     "h" '(nil :which-key "hierarchy")
     "hc" '(eglot-show-call-hierarchy :which-key "call-hierarchy")
     "ht" '(eglot-show-type-hierarchy :which-key "type-hierarchy")
     ;; rename
     "R" '(eglot-rename :which-key "rename"))))


(provide 'init-eglot)

;;; init-eglot.el ends here
