;; init-minibuffer.el --- minibuffer completions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  minibuffer completions
;;

;;; Code:

;; minibuffer ui
(use-package vertico
  :hook (after-init . vertico-mode)
  :bind ((:map vertico-map
               ("C-k" . vertico-previous)
               ("C-j" . vertico-next)
               ([backspace] . vertico-directory-delete-char)
               ([escape] . abort-recursive-edit)))
  :custom
  (vertico-resize nil)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vertico-mode 'emacs))
  :config
  (add-hook 'minibuffer-setup-hook (lambda () (setq completion-styles '(orderless))))
  (add-hook 'minibuffer-exit-hook (lambda () (setq completion-styles '(basic partial-completion emacs22)))))

(use-package orderless
  :config
  ;; pinyin
  (use-package pinyinlib
    :commands pinyinlib-build-regexp-string)
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; Helpful minibuffer annotations
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-align 'center))

;; similar to Swiper
(use-package consult
  :bind ("C-s" . consult-line)
  :commands (consult-buffer consult-imenu consult-line consult-grep consult-ripgrep)
  :config
  (setq consult-preview-key nil))

;; minibuffer actions and occur/export features
;; TODO: https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :defer t)

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
