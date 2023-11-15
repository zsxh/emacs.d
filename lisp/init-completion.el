;; init-completion.el --- commpletion configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Code Completion Configuations
;;

;;; Code:


(use-package company
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
        company-idle-delay 0.01 ; decrease delay before autocompletion popup shows
        company-echo-delay 0.01
        company-minimum-prefix-length 1
        ;; Only search same major mode buffers for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers t
        company-dabbrev-downcase nil    ; No downcase when completion.
        company-require-match nil ; Don't require match, so you can still move your cursor as expected.
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend) ; remove `company-echo-metadata-frontend' frontend
        company-backends '(company-capf company-files company-dabbrev)
        company-global-modes '(not shell-mode eshell-mode eaf-mode
                                   erc-mode message-mode help-mode
                                   helpful-mode gud-mode telega-chat-mode)
        company-format-margin-function 'company-detect-icons-margin
        company-icon-size '(auto-scale . 16))

  (with-eval-after-load 'company-eclim
    ;;  Stop eclim auto save.
    (setq company-eclim-auto-save nil))

  (with-eval-after-load 'company-files
    (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)")))

;; Popup documentation for completion candidates
(use-package company-posframe
  :if (and (>= emacs-major-version 26)
           (display-graphic-p))
  :hook (global-company-mode . company-posframe-mode)
  :bind (:map company-posframe-active-map
              ("C-h" . +company-posframe/quickhelp-toggle))
  :config
  (setq company-posframe-quickhelp-delay 0.2
        company-posframe-show-indicator nil
        company-posframe-show-metadata nil)

  (defun +company-posframe/quickhelp-auto-hide ()
    (unless (member this-command '(mwheel-scroll
                                   handle-switch-frame
                                   company-posframe-quickhelp-scroll-up
                                   company-posframe-quickhelp-scroll-down
                                   +company-posframe/quickhelp-toggle))
      (company-posframe-quickhelp-hide)
      (remove-hook 'pre-command-hook #'+company-posframe/quickhelp-auto-hide t)))

  (defun +company-posframe/quickhelp-toggle ()
    (interactive)
    (if (with-current-buffer company-posframe-quickhelp-buffer
          (and (frame-live-p posframe--frame)
               (frame-visible-p posframe--frame)))
        (progn
          (message "toggle hide!")
          (company-posframe-quickhelp-hide))
      (message "toggle show!")
      (company-posframe-quickhelp-show)
      (company-posframe-quickhelp-raise-frame)
      (add-hook 'pre-command-hook #'+company-posframe/quickhelp-auto-hide 0 t))))


(provide 'init-completion)

;;; init-completion.el ends here
