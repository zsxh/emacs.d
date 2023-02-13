;; init-lsp.el --- lsp	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  lsp
;;

;;; Code:

;; python dependencies
;; pip install --user --upgrade epc orjson sexpdata six
(use-package lsp-bridge
  :load-path "~/.emacs.d/submodules/lsp-bridge"
  :commands (lsp-bridge-mode +lsp/set-leader-keys)
  :config
  (setq lsp-bridge-enable-debug nil
        lsp-bridge-enable-log nil
        lsp-bridge-python-command (expand-file-name "~/.pyenv/versions/3.11.1/bin/python")
        lsp-bridge-get-project-path-by-filepath (lambda (filepath)
                                                  "Customize lsp-bridge get project root function"
                                                  (when-let ((root (+project/root nil (file-name-directory filepath))))
                                                    (expand-file-name root)))
        lsp-bridge-lookup-doc-tooltip-border-width 10
        lsp-bridge-signature-show-function (if (display-graphic-p) 'lsp-bridge-signature-posframe 'message)
        lsp-bridge-signature-posframe-params (list :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                                                   :internal-border-width 8
                                                   :max-width 60
                                                   :max-height 12)
        lsp-bridge-signature-help-fetch-idle 0.3
        lsp-bridge-diagnostic-tooltip-border-width 0
        lsp-bridge-code-action-preview-delay 0.5
        lsp-bridge-completion-obey-trigger-characters-p t
        ;; Disable unused acm backends, HACK: disable elisp backends
        acm-backend-elisp-symbols-update-timer -1
        lsp-bridge-enable-search-words nil
        lsp-bridge-enable-auto-format-code nil)


  ;; FIXME: temp fix java-ts-mode
  (add-to-list 'lsp-bridge-single-lang-server-mode-list (cons '(java-mode java-ts-mode) "jdtls"))

  (transient-define-prefix trainsient-scoll-popup-lsp-document ()
    ["scoll popup document"
     ("j" "scroll up" lsp-bridge-popup-documentation-scroll-up :transient t)
     ("k" "scrool down" lsp-bridge-popup-documentation-scroll-down :transient t)
     ("q" "quit" transient-quit-all)])

  (defun +lsp/lookup-document ()
    (interactive)
    (lsp-bridge-popup-documentation)
    (trainsient-scoll-popup-lsp-document))

  (defun +lsp/set-leader-keys (&optional map)
    (let ((mode-map (or map (keymap-symbol (current-local-map)))))
      ;; (message "debug: current local map: %s" mode-map)
      (+funcs/major-mode-leader-keys
       mode-map
       "A" '(lsp-bridge-code-action :which-key "code-action")
       "D" '(+lsp/lookup-document :which-key "hover:document")
       "e" '(nil :which-key "error")
       "el" '(lsp-bridge-diagnostic-list :which-key "list-error")
       "en" '(lsp-bridge-diagnostic-jump-next :which-key "next-error")
       "ep" '(lsp-bridge-diagnostic-jump-prev :which-key "prev-error")
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
      (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
      "q" 'lsp-bridge-ref-quit))

  ;; TODO: remap `lsp-bridge-call-hierarchy-mode-map'

  (define-key lsp-bridge-mode-map [remap evil-goto-definition] #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map [remap xref-go-back] #'lsp-bridge-find-def-return))

(use-package acm
  :ensure nil
  :defer t
  :bind ((:map acm-mode-map
               ("C-j" . acm-select-next)
               ("C-k" . acm-select-prev)
               ("TAB" . acm-insert-common)
               ("\t" . acm-insert-common)
               ("C-h" . acm-doc-toggle)))
  :config
  (setq acm-menu-length 10
        acm-candidate-match-function 'orderless-flex
        ;; backends
        acm-enable-tabnine nil
        acm-enable-telega nil
        acm-enable-search-file-words nil))


(provide 'init-lsp)

;;; init-lsp.el ends here
