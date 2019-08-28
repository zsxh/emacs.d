;; init-language-server.el --- language-server Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  language-server Configurations
;;

;;; Code:

;;;;;;;;;;;;;; Language Common Leader Keys ;;;;;;;;;;;;;;
(defvar-local emacs-lsp-client 'lsp-mode
  "'lsp-mode or 'eglot")

(defmacro +language-server/set-common-leader-keys (mode-map)
  `(progn
    (if (and (eq emacs-lsp-client 'lsp-mode)
             (not lsp-prefer-flymake))
        ;; flycheck
        (+funcs/major-mode-leader-keys
         ,mode-map
         "e" '(nil :which-key "error")
         "en" '(flycheck-next-error :which-key "next-error")
         "ep" '(flycheck-previous-error :which-key "prev-error"))
      ;; flymake
      (+funcs/major-mode-leader-keys
       ,mode-map
       "e" '(nil :which-key "error")
       "en" '(flymake-goto-next-error :which-key "next-error")
       "ep" '(flymake-goto-prev-error :which-key "prev-error")))

    (if (eq emacs-lsp-client 'lsp-mode)
        ;; lsp-mode
        (+funcs/major-mode-leader-keys
         ,mode-map
         "A" '(lsp-execute-code-action :which-key "code-action")
         "d" '(nil :which-key "debug")
         "db" '(dap-breakpoint-toggle :which-key "breakpoint-toggle")
         "dh" '(hydra-debugger-control/body :which-key "hydra-control")
         "dr" '(dap-debug :which-key "run")
         "D" '(lsp-describe-thing-at-point :which-key "describe-thing-at-point")
         "f" '(lsp-format-buffer :which-key "format")
         "g" '(nil :which-key "go")
         "gd" '(lsp-find-definition :which-key "find-definitions")
         "gi" '(lsp-find-implementation :which-key "find-implementation")
         "gr" '(lsp-find-references :which-key "find-references")
         "R" '(lsp-rename :which-key "rename"))
      ;; eglot
      (+funcs/major-mode-leader-keys
       ,mode-map
       "A" '(eglot-code-actions :which-key "code-action")
       "f" '(eglot-format :which-key "format")
       "g" '(nil :which-key "go")
       "gd" '(xref-find-definitions :which-key "find-definitions")
       "gr" '(xref-find-references :which-key "find-references")
       "R" '(eglot-rename :which-key "rename")))))

;;;;;;;;;;;;;; Eglot ;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :commands eglot-ensure)

;;;;;;;;;;;;;; Lsp-mode ;;;;;;;;;;;;;;

;; Lsp do not support temporary buffer yet
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(use-package lsp-mode
  :quelpa ((lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode"))
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil)
  :config
  (require 'lsp-clients)

  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        lsp-eldoc-render-all nil
        lsp-keep-workspace-alive t
        lsp-use-native-json t
        lsp-enable-symbol-highlighting nil
        lsp-eldoc-enable-signature-help nil)

  (setq lsp-links-check-internal 0.5
        lsp-lens-check-interval 0.2)

  (advice-add 'lsp :after
              (lambda ()
                (setq-local company-backends
                            '(company-lsp company-files company-dabbrev))))

  ;; (when (package-installed-p 'focus)
  ;;   (advice-add 'lsp :after (lambda () (focus-mode 1))))
  )

(use-package company-lsp
  :after (company lsp-mode)
  :ensure t)

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  ;; :preface (setq lsp-ui-doc-enable (display-graphic-p)
  ;;                lsp-ui-sideline-enable nil)
  :bind (:map lsp-ui-peek-mode-map
              ("j" . lsp-ui-peek--select-next)
              ("k" . lsp-ui-peek--select-prev)
              ("C-j" . lsp-ui-peek--select-next)
              ("C-k" . lsp-ui-peek--select-prev))
  :config
  (setq lsp-ui-doc-enable (display-graphic-p)
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point)

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

  ;; FIXME: https://emacs-china.org/t/xwidget-async/10207/6
  ;; async process won't be killed after enabling xwdiget
  ;; (when (featurep 'xwidget-internal)
  ;;   (setq lsp-ui-doc-use-webkit t))

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

  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-ignore-duplicate t)

  (set-face-foreground 'lsp-ui-sideline-code-action "#FF8C00"))

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
             (lsp)))
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


(provide 'init-language-server)

;;; init-language-server.el ends here
