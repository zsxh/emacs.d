;; init-minibuffer.el --- minibuffer completions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  minibuffer completions
;;

;;; Code:

(setq completion-ignore-case t)

(add-hook 'text-mode-hook
          (lambda ()
            (remove-hook
             'completion-at-point-functions
             'ispell-completion-at-point t)))

;; Completion Styles
;; NOTE: [Manual] https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; NOTE: [Article] https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; NOTE: [Video] https://www.youtube.com/watch?v=w9hHMDyF9V4
;; `completion-category-overrides' > `completion-category-defaults' > `completion-styles'

;; minibuffer ui
(use-package vertico
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind ((:map vertico-map
          ("C-k" . vertico-previous)
          ("C-j" . vertico-next)
          ("C-." . embark-act)
          ([backspace] . vertico-directory-delete-char)
          ([escape] . abort-recursive-edit)))
  :custom
  (vertico-resize nil)
  :config
  (with-eval-after-load 'evil
    (dolist (mode '(vertico-mode
                    vertico-buffer-mode
                    vertico-flat-mode
                    vertico-grid-mode
                    vertico-indexed-mode
                    vertico-mouse-mode
                    vertico-multiform-mode
                    vertico-reverse-mode
                    vertico-unobtrusive-mode))
      (evil-set-initial-state mode 'emacs))))

;; `orderless' completion style.
(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-overrides '((buffer (styles orderless))))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; pinyin
(use-package pinyinlib
  :after orderless
  :autoload completion--regex-pinyin
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str))))

;; Helpful minibuffer annotations
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-align 'left)
  :config
  (defun marginalia--time-absolute@advice (time)
    (let ((system-time-locale "C"))
      (format-time-string
       ;; `decoded-time-year' is only available on Emacs 27, use nth 5 here.
       (if (> (nth 5 (decode-time (current-time)))
              (nth 5 (decode-time time)))
           " %Y-%m-%d"
         "%m-%d %H:%M")
       time)))
  (advice-add 'marginalia--time-absolute :override #'marginalia--time-absolute@advice))

;; similar to Swiper
(use-package consult
  :bind ("C-s" . consult-line)
  :init
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function #'consult-xref)
  :commands (consult-buffer consult-imenu consult-line consult-grep consult-ripgrep consult--read consult-locate)
  :config
  (setq consult-preview-key 'any
        consult-async-min-input 2
        consult-line-start-from-top t
        consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer)))

;; minibuffer actions and occur/export features
;; NOTE: https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :defer t)

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode))


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
