;; init-minibuffer.el --- minibuffer completions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  minibuffer completions
;;

;;; Code:

;; minibuffer ui
;; FIXME: `vertico-directory-delete-char', tramp path
(use-package vertico
  :hook (after-init . vertico-mode)
  :bind ((:map vertico-map
               ("C-k" . vertico-previous)
               ("C-j" . vertico-next)
               ("~" . +vertico/insert-tilde)
               ("/" . +vertico/insert-slash)
               ([backspace] . vertico-directory-delete-char)
               ([escape] . abort-recursive-edit)))
  :custom
  (vertico-resize nil)
  (with-eval-after-load 'evil
    (dolist (mode '(vertico-mode
                    vertico-buffer-mode
                    vertico-flat-mode
                    vertico-grid-mode
                    vertico-indexed-mode
                    vertico-mouse-mode
                    vertico-multiform-mode
                    vertico-reverse-mode
                    vertico-unobtrusive-mode
                    vertico-posframe-mode))
      (evil-set-initial-state mode 'emacs)))
  :config
  (add-hook 'minibuffer-setup-hook (lambda () (setq completion-styles '(orderless))))
  (add-hook 'minibuffer-exit-hook (lambda () (setq completion-styles '(basic partial-completion emacs22))))

  (require 'vertico-directory)

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
  (marginalia-align 'left)
  :config
  ;; Format TIME as an absolute age
  (define-advice marginalia--time-absolute
      (:override (orig-fun &rest args) advice)
    (let ((system-time-locale "C")
          (time (nth 1 args)))
      (format-time-string
       ;; decoded-time-year is only available on Emacs 27, use nth 5 here.
       (if (> (nth 5 (decode-time (current-time)))
              (nth 5 (decode-time time)))
           " %Y-%m-%d"
         "%m-%d %H:%M")
       time))))

;; similar to Swiper
(use-package consult
  :bind ("C-s" . consult-line)
  :init
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function #'consult-xref)
  :commands (consult-buffer consult-imenu consult-line consult-grep consult-ripgrep consult--read consult-locate)
  :config
  (setq consult-preview-key 'any
        consult-async-min-input 2)
  ;; NOTE: https://www.reddit.com/r/emacs/comments/qk8akt/lsp_mode_and_consults_recent_files/
  (with-eval-after-load 'lsp-mode
    (defun advices/inhibit-if-non-essential (oldfun &rest args)
      "An around advice that inhibit OLDFUN if `non-essential' is non-nil."
      (unless non-essential
        (apply oldfun args)))
    (advice-add 'lsp-deferred :around #'advices/inhibit-if-non-essential)
    (advice-add 'lsp :around #'advices/inhibit-if-non-essential)))

;; minibuffer actions and occur/export features
;; TODO: https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :defer t)

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; (use-package minibuffer
;;   :ensure nil
;;   :bind (:map minibuffer-local-completion-map
;;               ("TAB" . minibuffer-force-complete)))


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
