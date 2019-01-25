;; init-language-server.el --- language-server Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  language-server Configurations
;;

;;; Code:

;;;;;;;;;;;;;; LANGUAGE SUPPORT ;;;;;;;;;;;;;;

;; require go language server `bingo'
;; https://github.com/saibing/bingo/wiki/Install
(use-package go-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook (go-mode . lsp))

;;;;;;;;;;;;;; Language Common Leader Keys ;;;;;;;;;;;;;;
(with-eval-after-load 'eglot
  (advice-add 'eglot-ensure :after '+language-server/set-leader-keys))
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp :after '+language-server/set-leader-keys))

(defun +language-server/set-leader-keys (&rest args)
  (let ((major-mode-map (intern (concat (symbol-name major-mode) "-map"))))
    (+language-server/set-normal-leader-keys major-mode-map)
    (+language-server/set-synax-check-leader-keys major-mode-map)))

(defun +language-server/set-synax-check-leader-keys (major-mode-map)
  (cond ((or
          (bound-and-true-p flymake-mode)
          (not (bound-and-true-p lsp-mode)))
         (+funcs/set-leader-keys-for-major-mode
          major-mode-map
          "e" '(nil :which-key "error")
          "en" '(flymake-goto-next-error :which-key "next-error")
          "ep" '(flymake-goto-prev-error :which-key "prev-error")))
        ((bound-and-true-p flycheck-mode)
         (with-eval-after-load 'flycheck
           (+funcs/set-leader-keys-for-major-mode
            major-mode-map
            "e" '(nil :which-key "error")
            "en" '(flycheck-next-error :which-key "next-error")
            "ep" '(flycheck-previous-error :which-key "prev-error"))))
        (t (message "[error] language server requires 'flymake or 'flycheck."))))

(defun +language-server/set-normal-leader-keys (major-mode-map)
  (cond ((bound-and-true-p lsp-mode)
         (+funcs/set-leader-keys-for-major-mode
          major-mode-map
          "A" '(lsp-execute-code-action :which-key "code-action")
          "f" '(lsp-format-buffer :which-key "format")
          "g" '(nil :which-key "go")
          "gd" '(lsp-find-definition :which-key "find-definitions")
          "gi" '(lsp-find-implementation :which-key "find-implementation")
          "gr" '(lsp-find-references :which-key "find-references")
          "R" '(lsp-rename :which-key "rename")))
        ((featurep 'eglot)
         (+funcs/set-leader-keys-for-major-mode
          major-mode-map
          "A" '(eglot-code-actions :which-key "code-action")
          "f" '(eglot-format :which-key "format")
          "g" '(nil :which-key "go")
          "gd" '(xref-find-definitions :which-key "find-definitions")
          "gr" '(xref-find-references :which-key "find-references")
          "R" '(eglot-rename :which-key "rename")))
        (t (message "[error] language server requires 'lsp-mode or 'eglot"))))

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
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-keep-workspace-alive nil)
  ;; FIXME: enable company-yasnippet, but can be messy
  (advice-add 'lsp :after
              (lambda ()
                (setq-local company-backends
                            '((company-lsp :separate company-yasnippet))))))

;; (use-package company-lsp
;;   :after (company lsp-mode)
;;   :ensure t)

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  ;; disable lsp-ui doc and sideline
  :preface (setq lsp-ui-doc-enable nil
                 lsp-ui-sideline-enable nil)
  :bind (:map lsp-ui-peek-mode-map
              ("j" . lsp-ui-peek--select-next)
              ("k" . lsp-ui-peek--select-prev)
              ("C-j" . lsp-ui-peek--select-next)
              ("C-k" . lsp-ui-peek--select-prev))
  :config
  (setq lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-ignore-duplicate t)
  (set-face-foreground 'lsp-ui-sideline-code-action "#FF8C00"))

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

;; FIXME: ipython doesn't work, use python instead
(defvar org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))

(add-to-list 'org-babel-lang-list (if (>= emacs-major-version 26) "shell" "sh"))

(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enbale ,lang)))


;;;;;;;;;;;;;; Debug Adapter Protocol for Emacs ;;;;;;;;;;;;;;

(use-package dap-mode
  :after lsp-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0))))

;; add minor mode(keybindings) for lsp debug
(with-eval-after-load 'dap-mode
  (defvar +dap/debug-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") 'dap-next)
      (define-key map (kbd "s") 'dap-step-in)
      (define-key map (kbd "o") 'dap-step-out)
      (define-key map (kbd "b") 'dap-breakpoint-toggle)
      (define-key map (kbd "B") 'dap-breakpoint-condition)
      (define-key map (kbd "c") 'dap-continue)
      (define-key map (kbd "C") 'dap-disconnect)
      map)
    "my dap mode debug keybindings")

  (define-minor-mode +dap/debug-mode
    "A minor mode for dap debug key settings."
    :init-value nil
    :keymap +dap/debug-mode-map)

  (define-global-minor-mode +dap/global-debug-mode +dap/debug-mode
    (lambda ()
      (when (memq major-mode '(java-mode python-mode))
        (+dap/debug-mode))))

  (with-eval-after-load 'evil
    (defun +dap/evil-debug-key-settings ()
      (evil-define-key 'normal +dap/debug-mode-map
        "n" 'dap-next
        "s" 'dap-step-in
        "o" 'dap-step-out
        "b" 'dap-breakpoint-toggle
        "B" 'dap-breakpoint-condition
        "c" 'dap-continue
        "C" 'dap-disconnect))
    (add-hook '+dap/debug-mode-hook #'+dap/evil-debug-key-settings))

  (defun +dap/debug-mode-disable (&optional args)
    (when +dap/global-debug-mode
      (+dap/global-debug-mode -1)
      (let ((inhibit-message t))
        (message "+dap/global-debug-mode disabled"))))

  (defun +dap/debug-mode-enable (&optional args)
    (unless +dap/global-debug-mode
      (+dap/global-debug-mode)
      ;; `evil-define-key' for minor mode does not take effect until a state transition
      ;; Issue: https://github.com/emacs-evil/evil/issues/301
      (when (bound-and-true-p evil-mode)
        (if (eq evil-state 'normal)
            (progn
              (evil-change-state 'emacs)
              (evil-change-state 'normal))
          (progn
            (let ((cur-state evil-state))
              (evil-change-state 'normal)
              (evil-change-state cur-state)))))
      (let ((inhibit-message t))
        (message "+dap/global-debug-mode enabled"))))

  ;; Manual toggle
  (defun +dap/debug-key-settings--toggle ()
    (interactive)
    (if +dap/debug-mode
        (+dap/debug-mode-disable)
      (+dap/debug-mode-enable)))

  ;; Auto toggle
  (add-hook 'dap-session-created-hook #'+dap/debug-mode-enable)
  (add-hook 'dap-terminated-hook #'+dap/debug-mode-disable))


(provide 'init-language-server)

;;; init-language-server.el ends here
