;; init-lsp.el --- language-server Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  language-server Configurations
;;

;;; Code:

;;;;;;;;;;;;;; Lsp-mode ;;;;;;;;;;;;;;

;; Performance problem
;; https://github.com/emacs-lsp/lsp-mode#performance
;;
;; lsp-org command
;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/page/lsp-org.md
(use-package lsp-mode
  :quelpa (lsp-mode :fetcher github
                    :repo "emacs-lsp/lsp-mode"
                    :files (:defaults "clients/*.el"))
  :commands (lsp lsp-deferred lsp-session lsp-session-folders lsp-org)
  ;; :init
  ;; (setq lsp-use-plists t)
  :config
  (setq lsp-auto-guess-root nil
        lsp-client-packages '()
        lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil ; turn off for better performance
        lsp-keep-workspace-alive nil
        lsp-idle-delay 1
        lsp-debounce-full-sync-notifications-interval 1.0
        lsp-diagnostics-provider :flycheck
        lsp-log-io nil
        ;; TODO: wait childframe rendering
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate t
        lsp-lens-enable t
        lsp-completion-sort-initial-results nil ; do not resort the result
        ;; lsp-completion--no-reordering t ; do not resort the result
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-restart 'ignore)

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.json$" lsp-file-watch-ignored) ; json

  (when (bound-and-true-p read-process-output-max)
      (setq read-process-output-max (* 1024 1024)))

  (use-package lsp-lens
    :ensure nil
    :config
    (setq lsp-lens-debounce-interval 1.5))

  (use-package lsp-completion
    :ensure nil
    :config
    (setq lsp-completion-provider :capf))

  (use-package lsp-diagnostics
    :ensure nil
    :config
    ;; lsp flycheck faces
    (setq lsp-diagnostics-attributes '((deprecated :strike-through t))))

  ;; https://emacs-lsp.github.io/lsp-mode/page/faq/
  ;; How do I force lsp-mode to forget the workspace folders for multi root servers
  ;; so the workspace folders are added on demand?
  ;; FIXME: first buffer in another project need to manually call lsp
  (advice-add 'lsp :before (lambda (&rest _args)
                             (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  ;; Increase the amount of data which Emacs reads from the process.
  ;; Again the emacs default is too low 4k considering that the some
  ;; of the language server responses are in 800k - 3M range.
  ;; New variable 'read-process-ouput-max' controls sub-process throught since emacs27
  (defun +lsp/setup ()
    ;; (unless (member major-mode '(c-mode c++-mode java-mode))
    ;;   (lsp-lens-mode))
    (lsp-lens-mode))

  (add-hook 'lsp-managed-mode-hook '+lsp/setup)

  (defun +lsp/update-server ()
    (interactive)
    (lsp-install-server t)))

(use-package lsp-ui
  :after lsp-mode
  :preface (setq lsp-ui-doc-enable nil
                 lsp-ui-sideline-enable nil)
  :bind ((:map lsp-ui-mode-map
               ([remap evil-goto-definition] . lsp-ui-peek-find-definitions)
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references)
               ("C-M-g" . lsp-ui-peek-find-definitions)
               ("C-M-r" . lsp-ui-peek-find-references)
               ("C-M-p" . lsp-ui-peek-jump-backward)
               ("C-M-n" . lsp-ui-peek-jump-forward))
         (:map lsp-ui-peek-mode-map
               ("j" . lsp-ui-peek--select-next)
               ("k" . lsp-ui-peek--select-prev)
               ("C-j" . lsp-ui-peek--select-next)
               ("C-k" . lsp-ui-peek--select-prev)))
  :config
  (setq lsp-ui-doc-delay 0.5
        lsp-ui-doc-header t
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
       "el" '(+flycheck/popup-errors :which-key "popup-errors")
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
       "dc" '(dap-breakpoint-condition :which-key "breakpoint-condition")
       "dh" '(hydra-debugger-control/body :which-key "hydra-control")
       "dl" '(dap-breakpoint-log-message :which-key "breakpoint-log-message")
       "dr" '(dap-debug :which-key "run")
       ;; FIXME: lsp-ui-doc-glance scoll down doc will freeze emacs
       ;; "D" '(lsp-ui-doc-glance :which-key "lsp-ui-doc-glance")
       "D" '(+lsp/toggle-doc-show :which-key "toggle-doc-hover")
       "f" '(lsp-format-buffer :which-key "format")
       "g" '(nil :which-key "go")
       "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
       "gD" '(lsp-describe-thing-at-point :which-key "describe-thing-at-point")
       "gi" '(lsp-ui-peek-find-implementation :which-key "find-implementation")
       "gr" '(lsp-ui-peek-find-references :which-key "find-references")
       "l" '(lsp-avy-lens :which-key "Click lens using avy")
       "R" '(lsp-rename :which-key "rename"))))))

;;;;;;;;;;;;;; Eglot ;;;;;;;;;;;;;;

(use-package eglot
  :commands eglot-ensure
  :config
  (add-hook 'eglot--managed-mode-hook (lambda ()
                                        (when (bound-and-true-p read-process-output-max)
                                          (setq-local read-process-output-max (* 1024 1024))))))


(provide 'init-lsp)

;;; init-lsp.el ends here
