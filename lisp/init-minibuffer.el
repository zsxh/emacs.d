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
               ("~" . +vertico/insert-tilde)
               ;; ;; TODO: improve find file root path and protocols
               ("/" . +vertico/insert-slash)
               ([backspace] . vertico-directory-delete-char)
               ([escape] . abort-recursive-edit)))
  :custom
  (vertico-resize nil)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vertico-mode 'emacs))
  :config
  (add-hook 'minibuffer-setup-hook (lambda () (setq completion-styles '(orderless))))
  (add-hook 'minibuffer-exit-hook (lambda () (setq completion-styles '(basic partial-completion emacs22))))

  (defun +vertico/directory-home ()
    (when (and (> (point) (minibuffer-prompt-end))
               (eq (char-before) ?/)
               (vertico-directory--completing-file-p))
      (delete-minibuffer-contents)
      (insert "~/")
      t))
  (defun +vertico/insert-tilde ()
    (interactive)
    (unless (+vertico/directory-home)
      (insert "~")))
  (defun +vertico/insert-slash ()
    (interactive)
    (when (and (> (point) (minibuffer-prompt-end))
               (eq (char-before) ?/)
               (save-excursion
                 (goto-char (1- (point)))
                 (eq (char-before) ?/))
               (vertico-directory--completing-file-p))
      (delete-minibuffer-contents))
    (insert "/")))

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

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-border-width 5)

  (defun +vertico/custom-posframe-poshandler (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (plist-get info :posframe-height))
             3)))
  (setq vertico-posframe-poshandler #'+vertico/custom-posframe-poshandler)

  ;; (defun +vertico/posframe-get-fixed-size ()
  ;;   "Set the vertico-posframe size according to the current frame."
  ;;   (let ((height (or vertico-posframe-height 11))
  ;;         (width (min (or vertico-posframe-width 200) (round (* 0.62 (frame-width))))))
  ;;     (list :height height :width width :min-height height :min-width width)))

  ;; (setq vertico-posframe-size-function '+vertico/posframe-get-fixed-size)
  )


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
