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
;; (setq lsp-use-plists t)
;;
;; TODO: check it out, https://github.com/iocanel/idee
(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github
  ;;                   :repo "emacs-lsp/lsp-mode"
  ;;                   :files (:defaults "clients/*.el"))
  :commands (lsp lsp-deferred lsp-session lsp-session-folders lsp-org)
  :custom
  (lsp-server-install-dir (locate-user-emacs-file "cache/lsp"))
  :config
  (setq lsp-auto-guess-root nil
        lsp-client-packages '()
        lsp-keep-workspace-alive nil
        lsp-prefer-capf t
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil ; turn off for better performance
        lsp-idle-delay 1.5
        lsp-debounce-full-sync-notifications-interval 2.0
        lsp-diagnostics-provider :flycheck
        lsp-log-io nil
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        ;; TODO: keybindings for signature show up
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate '(:on-trigger-char :on-server-request)
        lsp-signature-function 'lsp-signature-posframe
        lsp-lens-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-restart 'ignore
        lsp-session-file (expand-file-name "cache/lsp-session" user-emacs-directory)
        ;; Disable features that have great potential to be slow.
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        ;; Reduce unexpected modifications to code
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-response-timeout 10)

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.json$" lsp-file-watch-ignored-files) ; json

  ;; Increase the amount of data which Emacs reads from the process.
  ;; Again the emacs default is too low 4k considering that the some
  ;; of the language server responses are in 800k - 3M range.
  ;; New variable 'read-process-ouput-max' controls sub-process throught since emacs27
  (when (bound-and-true-p read-process-output-max)
    (setq read-process-output-max (* 1024 1024)))

  (use-package lsp-lens
    :ensure nil
    :config
    (setq lsp-lens-debounce-interval 1.5))

  ;; Enable code lens
  ;; (add-hook 'lsp-configure-hook
  ;;           (lambda ()
  ;;             (cond
  ;;              ((eq major-mode 'java-mode)
  ;;               (progn
  ;;                 (lsp-lens-mode)
  ;;                 (lsp-java-lens-mode)))
  ;;              ((eq major-mode 'rust-mode)
  ;;               (lsp-lens-mode))
  ;;              (t nil))))
  (defun +lsp/enable-or-avy-lens ()
    "Manually toggle code lens in specific buffer."
    (interactive)
    (if lsp-lens-mode
        (lsp-avy-lens)
      (lsp-lens-mode)))

  (use-package lsp-completion
    :ensure nil
    :config
    (setq lsp-completion-sort-initial-results t ; do not resort the result
          lsp-completion-provider :capf
          lsp-completion-default-behaviour :insert
          lsp-completion-show-detail t
          lsp-completion-no-cache nil
          lsp-completion-use-last-result t))

  (use-package lsp-diagnostics
    :ensure nil
    :config
    ;; lsp flycheck faces
    (setq lsp-diagnostics-attributes '((deprecated :strike-through t))))

  ;; > What are workspaces in emacs-lsp? What about multi-root?
  ;;   https://github.com/emacs-lsp/lsp-mode/discussions/3095
  ;; > How do I force lsp-mode to forget the workspace folders for multi root servers
  ;;   so the workspace folders are added on demand?
  ;;   https://emacs-lsp.github.io/lsp-mode/page/faq/
  (advice-add 'lsp :before (lambda (&rest _args)
                             (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  ;; Code from doom-emacs
  (defvar +lsp/defer-shutdown 10
    "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")
  (defvar +lsp/deferred-shutdown-timer nil)
  (defun +lsp/defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp/defer-shutdown)
            (= +lsp/defer-shutdown 0))
        (funcall orig-fn restart)
      (when (timerp +lsp/deferred-shutdown-timer)
        (cancel-timer +lsp/deferred-shutdown-timer))
      (setq +lsp/deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp/defer-shutdown) +lsp/defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall orig-fn)))))
             lsp--cur-workspace))))
  (advice-add #'lsp--shutdown-workspace :around #'+lsp/defer-server-shutdown-a)

  (with-eval-after-load 'evil
    (evil-define-key 'normal lsp-log-io-mode-map "k" 'evil-previous-line)))

(use-package lsp-ui
  :after lsp-mode
  :preface (setq lsp-ui-doc-enable nil
                 lsp-ui-sideline-enable nil)
  :bind ((:map lsp-ui-mode-map
               ([remap evil-goto-definition] . lsp-find-definition)
               ([remap xref-find-definitions] . lsp-find-definition)
               ([remap xref-find-references] . lsp-ui-peek-find-references)
               ("C-M-g" . lsp-find-definition)
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
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-webkit nil
        lsp-ui-peek-show-directory nil)

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

  (defun +lsp/lsp-ui-doc--glance-hide-frame-a (&rest _)
    (not (member this-command '(ignore
                                mwheel-scroll
                                handle-switch-frame
                                dap-tooltip-mouse-motion))))
  (advice-add 'lsp-ui-doc--glance-hide-frame :before-while '+lsp/lsp-ui-doc--glance-hide-frame-a)

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
       "el" '(lsp-treemacs-errors-list :which-key "lsp-treemacs-errors-list")
       "en" '(flymake-goto-next-error :which-key "next-error")
       "ep" '(flymake-goto-prev-error :which-key "prev-error")))
     (t
      ;; flycheck
      (+funcs/major-mode-leader-keys
       mode-map
       "e" '(nil :which-key "error")
       "el" '(lsp-treemacs-errors-list :which-key "lsp-treemacs-errors-list")
       ;; "el" '(+flycheck/popup-errors :which-key "popup-errors")
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
       "dh" '(dap-hydra :which-key "dap-hydra")
       "dl" '(dap-breakpoint-log-message :which-key "breakpoint-log-message")
       "dr" '(dap-debug :which-key "run")
       "D" '(lsp-ui-doc-glance :which-key "lsp-ui-doc-glance")
       "f" '(lsp-format-buffer :which-key "format")
       "g" '(nil :which-key "go")
       "gd" '(lsp-find-definition :which-key "find-definitions")
       "gD" '(lsp-describe-thing-at-point :which-key "describe-thing-at-point")
       "gi" '(lsp-ui-peek-find-implementation :which-key "find-implementation")
       "gr" '(lsp-ui-peek-find-references :which-key "find-references")
       "gs" '(lsp-ui-find-workspace-symbol :which-key "find-workspace-symbol")
       "l" '(nil :which-key "list")
       "L" '(+lsp/enable-or-avy-lens :which-key "Enable/Avy lens")
       "R" '(lsp-rename :which-key "rename"))))))

;; TODO: `consult-lsp' https://github.com/gagbo/consult-lsp
;; (use-package consult-lsp
;;   :defer t)


;;;;;;;;;;;;;; Eglot ;;;;;;;;;;;;;;

(use-package eglot
  :commands eglot-ensure
  :config
  (add-hook 'eglot--managed-mode-hook (lambda ()
                                        (when (bound-and-true-p read-process-output-max)
                                          (setq-local read-process-output-max (* 1024 1024))))))


(provide 'init-lsp)

;;; init-lsp.el ends here
