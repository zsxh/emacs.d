;; init-language-server.el --- language-server Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  language-server Configurations
;;

;;; Code:

;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some
;; of the language server responses are in 800k - 3M range.
;;
;; New variable 'read-process-ouput-max' controls sub-process throught since emacs27
;;
(when (bound-and-true-p read-process-output-max)
  (setq read-process-output-max (* 1024 1024)))

;;;;;;;;;;;;;; Eglot ;;;;;;;;;;;;;;

(use-package eglot
  :commands eglot-ensure)

;;;;;;;;;;;;;; Lsp-mode ;;;;;;;;;;;;;;

;; Performance problem
;; https://github.com/emacs-lsp/lsp-mode#performance
;;
;; Support temporary buffer
;; https://github.com/emacs-lsp/lsp-mode/issues/377
;;
(use-package lsp-mode
  :quelpa ((lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode"))
  :commands (lsp lsp-deferred lsp-session lsp-session-folders)
  :config
  (setq lsp-auto-guess-root nil
        lsp-client-packages '()
        lsp-keep-workspace-alive nil
        lsp-diagnostic-package :flycheck
        lsp-prefer-capf nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil ; turn off for better performance
        lsp-eldoc-render-all nil
        lsp-keep-workspace-alive nil
        lsp-links-check-internal 1
        lsp-lens-check-interval 1
        lsp-lens-debounce-interval 1.5
        lsp-idle-delay 1)

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.json$" lsp-file-watch-ignored) ; json

  (defun +lsp/setup ()
    (unless (member major-mode '(c-mode c++-mode java-mode))
      (lsp-lens-mode)))

  (add-hook 'lsp-after-open-hook '+lsp/setup)

  (with-eval-after-load 'evil
    (define-key lsp-mode-map [remap evil-goto-definition] 'lsp-find-definition))

  (defun +lsp/update-server ()
    (interactive)
    (lsp-install-server t)))

(use-package company-lsp
  :after (company lsp-mode)
  :custom
  (company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :after lsp-mode
  :preface (setq lsp-ui-doc-enable nil
                 lsp-ui-sideline-enable nil)
  :bind ((:map lsp-ui-mode-map
               ("C-M-g" . lsp-ui-peek-find-definitions)
               ("C-M-r" . lsp-ui-peek-find-references)
               ("C-M-q" . lsp-ui-peek-jump-backward)
               ("C-M-n" . lsp-ui-peek-jump-forward))
         (:map lsp-ui-peek-mode-map
               ("j" . lsp-ui-peek--select-next)
               ("k" . lsp-ui-peek--select-prev)
               ("C-j" . lsp-ui-peek--select-next)
               ("C-k" . lsp-ui-peek--select-prev)))
  :config
  (setq lsp-ui-doc-delay 0.5
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        ;; FIXME: https://emacs-china.org/t/xwidget-async/10207/6
        ;; async process won't be killed after enabling xwdiget
        ;; lsp-ui-doc-use-webkit (featurep 'xwidget-internal)
        )

  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))

  (when (featurep 'doom-themes)
    (set-face-background 'lsp-ui-doc-background (doom-color 'bg-alt)))

  (defun +lsp/lsp-ui-doc--make-request-advice nil
    "Request the documentation to the LS."
    (when (and (not (bound-and-true-p lsp-ui-peek-mode))
               (lsp--capability "hoverProvider"))
      (-if-let (bounds (and (not (memq (char-after) '(?  ?\t ?\n ?\) ?\] ?\})))
                            (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                                (and (looking-at "[[:graph:]]") (cons (point) (1+ (point)))))))
          (unless (equal lsp-ui-doc--bounds bounds)
            (lsp--send-request-async
             (lsp--make-request "textDocument/hover" (lsp--text-document-position-params))
             (lambda (hover) (lsp-ui-doc--callback hover bounds (current-buffer)))))
        (lsp-ui-doc--hide-frame))))

  (advice-add 'lsp-ui-doc--make-request :override '+lsp/lsp-ui-doc--make-request-advice)

  (defun +lsp/toggle-doc-show ()
    "Popup/Hide hover information"
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (message "lsp-ui-doc disabled")
          (lsp-ui-doc-hide)
          (lsp-ui-doc-mode -1))
      (message "lsp-ui-doc enabled")
      (lsp-ui-doc-mode 1)
      (lsp-ui-doc-show)))

  (setq lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-ignore-duplicate t)

  (set-face-foreground 'lsp-ui-sideline-code-action "#FF8C00"))

;; FIXME: not working now
(when (featurep 'lsp-mode)
  ;; Support LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    ;; (cl-check-type lang symbolp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((lsp-file (or (->> info caddr (alist-get :lspfile))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (lsp-deferred)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  ;; lsp support org code block editing
  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java" "julia"
      "jupyter-python" "jupyter-julia" "jupyter-javascript"))

  (add-to-list 'org-babel-lang-list (if (>= emacs-major-version 26) "shell" "sh"))

  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

;;;;;;;;;;;;;; Language Common Leader Keys ;;;;;;;;;;;;;;
(defun +language-server/set-common-leader-keys (mode-map &optional eglot-p flymake-p)
  "Set major mode keybindings for modes using lsp/eglot.
`mode-map' is keymap symbol or literal keymap name."
  (let ((mode-map (if (symbolp mode-map)
                      mode-map
                    (keymap-symbol mode-map))))
     (cond
      (flymake-p
       ;; flymake
       (+funcs/major-mode-leader-keys
        mode-map
        "e" '(nil :which-key "error")
        "en" '(flymake-goto-next-error :which-key "next-error")
        "ep" '(flymake-goto-prev-error :which-key "prev-error")))
      (t
       ;; flycheck
       (+funcs/major-mode-leader-keys
        mode-map
        "e" '(nil :which-key "error")
        "en" '(flycheck-next-error :which-key "next-error")
        "ep" '(flycheck-previous-error :which-key "prev-error"))))

     (cond
      (eglot-p
       ;; eglot
       (+funcs/major-mode-leader-keys
        mode-map
        "A" '(eglot-code-actions :which-key "code-action")
        "f" '(eglot-format :which-key "format")
        "g" '(nil :which-key "go")
        "gd" '(xref-find-definitions :which-key "find-definitions")
        "gr" '(xref-find-references :which-key "find-references")
        "R" '(eglot-rename :which-key "rename")))
      (t
       ;; lsp-mode
       (+funcs/major-mode-leader-keys
        mode-map
        "A" '(lsp-execute-code-action :which-key "code-action")
        "d" '(nil :which-key "debug")
        "db" '(dap-breakpoint-toggle :which-key "breakpoint-toggle")
        "dh" '(hydra-debugger-control/body :which-key "hydra-control")
        "dr" '(dap-debug :which-key "run")
        "D" '(+lsp/toggle-doc-show :which-key "toggle-doc-hover")
        "f" '(lsp-format-buffer :which-key "format")
        "g" '(nil :which-key "go")
        "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
        "gD" '(lsp-describe-thing-at-point :which-key "describe-thing-at-point")
        "gi" '(lsp-ui-peek-find-implementation :which-key "find-implementation")
        "gr" '(lsp-ui-peek-find-references :which-key "find-references")
        "l" '(lsp-avy-lens :which-key "Click lens using avy")
        "R" '(lsp-rename :which-key "rename"))))))


(provide 'init-language-server)

;;; init-language-server.el ends here
