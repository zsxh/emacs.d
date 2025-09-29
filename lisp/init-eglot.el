;; init-eglot.el --- language server protocol client eglot	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Language Server Protocol Emacs Client Eglot
;;

;;; Code:

;; TODO: https://github.com/sqls-server/sqls

;; TODO: lsp semantic tokens
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=79374
;; https://github.com/lua-vr/eglot-semtok

;; NOTE: https://joaotavora.github.io/eglot/
;; NOTE: macos file descriptors limitation, https://www.reddit.com/r/emacs/comments/x4p7mg/fix_annoying_max_open_files_for_emacs/
;; NOTE: https://github.com/microsoft/vscode-extension-samples
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :foldingRangeProvider)
        eglot-stay-out-of '()
        ;; NOTE: drop log to improve performance
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-report-progress nil
        eglot-stay-out-of '()
        eglot-extend-to-xref t
        ;; https://github.com/joaotavora/eglot/issues/1491
        ;; https://github.com/microsoft/vscode-extension-samples/blob/main/code-actions-sample/README.md
        eglot-code-action-indications '()
        eglot-advertise-cancellation nil)
  (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs))

(with-eval-after-load 'eglot
  (cl-defgeneric eglot-execute (server action)
    "Ask SERVER to execute ACTION.
ACTION is an LSP `CodeAction', `Command' or `ExecuteCommandParams'
object."
    (:method
     (server action) "Default implementation."
     (eglot--dcase action
       (((Command) title command arguments)
        (cond
         ((string-prefix-p "java." command) (+java/execute-command server command arguments))
         ((string-prefix-p "moonbit" command) (+moonbit/execute-command server command arguments))
         (t (progn
              ;; Convert to ExecuteCommandParams and recurse (bug#71642)
              (cl-remf action :title)
              (eglot-execute server action)))))
       (((ExecuteCommandParams))
        (eglot--request server :workspace/executeCommand action))
       (((CodeAction) edit command data)
        (if (and (null edit) (null command) data
                 (eglot-server-capable :codeActionProvider :resolveProvider))
            (eglot-execute server (eglot--request server :codeAction/resolve action))
          (when edit (eglot--apply-workspace-edit edit this-command))
          (when command
            ;; Recursive call with what must be a Command object (bug#71642)
            (eglot-execute server command)))))))

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
     "hc" '(eglot-show-call-hierarchy :which-key "call-hierarchy")
     "ht" '(eglot-show-type-hierarchy :which-key "type-hierarchy")
     ;; rename
     "R" '(eglot-rename :which-key "rename"))))

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
  :vc (:url "https://github.com/Gavinok/eglot-codelens.git")
  :hook (eglot-managed-mode . eglot-codelens-mode)
  :config
  (define-advice eglot-codelens-apply-code-lenses (:override nil advice)
    "Request and display code lenses using Eglot."
    (save-excursion
      (let* ((code-lenses
              ;; request code lenses from the server
              (jsonrpc-request
               (eglot--current-server-or-lose)
               :textDocument/codeLens
               (list :textDocument (eglot--TextDocumentIdentifier))))
             (line-code-lenses (seq-group-by (lambda (item)
                                               (let* ((range (plist-get item :range))
                                                      (start (plist-get range :start))
                                                      (line (plist-get start :line)))
                                                 line))
                                             code-lenses)))
        (dolist (line-lenses line-code-lenses)
          (let* ((lenses (cdr line-lenses))
                 (size (length lenses)))
            (seq-map-indexed (lambda (lens idx)
                               (eglot-codelens-make-overlay-for-lens
                                ;; Construct a lens with a resolved command
                                ;; TODO consider just modifying the lens if that is faster
                                (or (cl-getf lens :command)
                                    (cl-getf (jsonrpc-request (eglot--current-server-or-lose)
                                                              :codeLens/resolve
                                                              lens)
                                             :command))
                                (cl-getf lens :range)
                                idx
                                (= idx (- size 1))))
                             lenses))))))

  (defun eglot-codelens-overlay-pos-and-indent-str (start-line)
    (let ((indent-str nil)
          (bol-pos nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line start-line)
        (beginning-of-line)
        (setq bol-pos (point))
        (if (looking-at "\s*")
            (setq indent-str (match-string 0))
          (setq indent-str "")))
      (cons bol-pos indent-str)))

  (define-advice eglot-codelens-make-overlay-for-lens (:override (command range priority last-elt-p) advice)
    "Insert overlays for each corresponding lens's COMMAND and RANGE."
    (let* ((start-line (thread-first
                         range
                         (cl-getf :start)
                         (cl-getf :line)))
           (bol-pos-and-indent-str
            (eglot-codelens-overlay-pos-and-indent-str start-line))
           (bol-pos (car bol-pos-and-indent-str))
           (indent-str (cdr bol-pos-and-indent-str))
           (ol (make-overlay bol-pos bol-pos))
           (text (concat
                  indent-str
                  (propertize (cl-getf command :title)
                              'face 'eglot-parameter-hint-face
                              'cursor t
                              'pointer 'hand
                              'mouse-face 'highlight
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map [mouse-1]
                                          (lambda () (interactive)
                                            (eglot-codelens-execute command)))
                                        map))
                  (if last-elt-p
                      "\n"
                    (propertize "| " 'face 'eglot-parameter-hint-face)))))
      ;; Try to combine all the lenses into a single overlay so we can
      ;; use this text property to prevent the cursor from ending up on
      ;; the right side of the overlay
      ;; taken from [[file:~/.emacs.d/elpa/flymake-1.3.7/flymake.el::put-text-property 0 1 'cursor t summary][eol overlays from flymake]]
      (put-text-property 0 1 'cursor t text)
      (overlay-put ol 'before-string text)
      (overlay-put ol 'eglot-codelens-overlay (list :command command
                                                    :range range))
      (overlay-put ol 'cursor-face 'error)
      (overlay-put ol 'priority priority)
      (add-to-list 'eglot-codelens-overlays ol))))

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


(provide 'init-eglot)

;;; init-eglot.el ends here
