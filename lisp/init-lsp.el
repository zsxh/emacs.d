;; init-lsp.el --- lsp	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  lsp
;;

;;; Code:

(use-package lsp-bridge
  :load-path "~/.emacs.d/submodules/lsp-bridge"
  :commands (lsp-bridge-mode +lsp/set-leader-keys)
  :config
  (setq
   lsp-bridge-enable-debug nil
   lsp-bridge-enable-log nil
   lsp-bridge-python-command (expand-file-name "~/.pyenv/versions/3.10.5/bin/python")
   lsp-bridge-get-project-path-by-filepath (lambda (filepath)
                                             "Customize lsp-bridge get project root function"
                                             (when-let ((root (+project/root nil (directory-file-name filepath))))
                                               (expand-file-name root)))
   lsp-bridge-lookup-doc-tooltip-border-width 10)

  (transient-define-prefix trainsient-scoll-popup-lsp-document ()
    ["scoll popup document"
     ("j" "scroll up" lsp-bridge-popup-documentation-scroll-up :transient t)
     ("k" "scrool down" lsp-bridge-popup-documentation-scroll-down :transient t)
     ("q" "quit" transient-quit-all)])

  (defun +lsp/lookup-document ()
    (interactive)
    (lsp-bridge-lookup-documentation)
    (trainsient-scoll-popup-lsp-document))

  (defun +lsp/set-leader-keys (&optional map)
    (let ((mode-map (or map (keymap-symbol (current-local-map)))))
      ;; (message "debug: current local map: %s" mode-map)
      (+funcs/major-mode-leader-keys
       mode-map
       "A" '(lsp-bridge-code-action :which-key "code-action")
       "D" '(+lsp/lookup-document :which-key "hover:document")
       "e" '(nil :which-key "error")
       "el" '(lsp-bridge-list-diagnostics :which-key "list-error")
       "en" '(lsp-bridge-jump-to-next-diagnostic :which-key "next-error")
       "ep" '(lsp-bridge-jump-to-prev-diagnostic :which-key "prev-error")
       "f" '(lsp-bridge-code-format :which-key "format")
       "g" '(nil :which-key "goto")
       "gd" '(lsp-bridge-find-def :which-key "find-definitions")
       "gi" '(lsp-bridge-find-impl :which-key "find-implementation")
       "gr" '(lsp-bridge-find-references :which-key "find-references")
       "R" '(lsp-bridge-rename :which-key "rename")
       "S" '(lsp-bridge-signature-help-fetch :which-key "signature-help-fetch"))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal lsp-bridge-ref-mode-map
      "j" 'lsp-bridge-ref-jump-next-keyword
      "k" 'lsp-bridge-ref-jump-prev-keyword
      "C-j" 'lsp-bridge-ref-jump-next-file
      "C-k" 'lsp-bridge-ref-jump-prev-file
      (kbd "RET")'lsp-bridge-ref-open-file-and-stay
      "q" 'lsp-bridge-ref-quit))

  (define-key lsp-bridge-mode-map [remap evil-goto-definition] #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map [remap xref-go-back] #'lsp-bridge-return-from-def))

(use-package acm
  :ensure nil
  :defer t
  :bind ((:map acm-mode-map
               ("C-j" . acm-select-next)
               ("C-k" . acm-select-prev)
               ("TAB" . acm-insert-common)
               ("\t" . acm-insert-common))))


(provide 'init-lsp)

;;; init-lsp.el ends here
