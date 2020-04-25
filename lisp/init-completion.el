;; init-completion.el --- commpletion configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Code Completion Configuations
;;

;;; Code:

(setq completion-ignore-case t)

(use-package company
  ;; :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("M-/" . yas-expand)
         ("C-c C-y" . company-yasnippet)
         (:map company-active-map
               ("M-n" . nil)
               ("M-p" . nil)
               ("C-k" . company-select-previous)
               ("C-j" . company-select-next)
               ("TAB" . company-complete-common)
               ("<tab>" . company-complete-common)
               ("<backtab>" . company-select-previous))
         (:map company-search-map
               ("M-n" . nil)
               ("M-p" . nil)
               ("C-k" . company-select-previous)
               ("C-j" . company-select-next)))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-tooltip-maximum-width (/ (frame-width) 2)
        company-idle-delay 0 ; decrease delay before autocompletion popup shows
        company-echo-delay (if (display-graphic-p) nil 0) ; remove annoying blinking
        company-minimum-prefix-length 1
        ;; company-require-match nil
        ;; company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil    ; No downcase when completion.
        company-require-match nil ; Don't require match, so you can still move your cursor as expected.
        company-backends '(company-capf company-files company-dabbrev)
        company-global-modes '(not shell-mode eshell-mode eaf-mode))
  (with-eval-after-load 'company-eclim
    ;;  Stop eclim auto save.
    (setq company-eclim-auto-save nil)))

(use-package flx
  :defer t)

(use-package company-fuzzy
  :defer t
  :config
  (with-eval-after-load 'elisp-mode
    (add-hook 'emacs-lisp-mode-hook 'company-fuzzy-mode)
    (add-hook 'lisp-interaction-mode-hook 'company-fuzzy-mode)))

(use-package prescient :defer t)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  (with-eval-after-load 'snippet
    (+funcs/major-mode-leader-keys
     snippet-mode-map
     "t" '(yas-tryout-snippet :which-key "yas-tryout-snippet"))))

(use-package company-posframe
  :if (and (>= emacs-major-version 26)
           (display-graphic-p))
  :after company
  :hook (global-company-mode . company-posframe-mode)
  :config
  (setq company-posframe-quickhelp-delay 0.3
        company-posframe-show-indicator t
        company-posframe-show-metadata t))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :if (or (< emacs-major-version 26)
          (not (display-graphic-p)))
  :after company
  :bind ((:map company-active-map
               ("M-h" . company-quickhelp-manual-begin)))
  :hook (global-company-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.3))

(use-package company-quickhelp-terminal
  :if (not (display-graphic-p))
  :after company
  :hook (global-company-mode . company-quickhelp-terminal-mode))


(provide 'init-completion)

;;; init-completion.el ends here
