;; init-completion.el --- commpletion configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Code Completion Configuations
;;

;;; Code:

(use-package company
  :ensure t
  ;; :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("M-/" . yas-expand)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("TAB" . company-complete-common)
         ("<tab>" . company-complete-common)
         ("<backtab>" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2 ; decrease delay before autocompletion popup shows
        ;; company-echo-delay 0  ; remove annoying blinking
        company-minimum-prefix-length 1
        ;; company-require-match nil
        ;; company-dabbrev-ignore-case nil
        ;; company-dabbrev-downcase nil
        ))

;; Popup documentation for completion candidates
(when (display-graphic-p)
  (use-package company-quickhelp
    :after company
    :ensure t
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :config (setq company-quickhelp-delay 0.3)))

;; (use-package company-flx
;;   :after company
;;   :ensure t
;;   :config (company-flx-mode 1))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (with-eval-after-load 'snippet
    (+funcs/set-leader-keys-for-major-mode
     'snippet-mode-map
     "t" '(yas-tryout-snippet :which-key "yas-tryout-snippet"))))


(provide 'init-completion)

;;; init-completion.el ends here
