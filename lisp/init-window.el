;; init-window.el --- Window Setup	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Window Setup
;;

;;; Code:

;; TODO: `display-buffer-alist' https://youtu.be/E-xUNlZi3rI?t=948
;; TODO: Make an Emacs Buffer Open the Way You Want, https://lambdaland.org/posts/2022-12-27_repl_buffer_on_the_right/

(use-package popper
  ;; TODO: popper keybinds
  ;; :bind (("C-`" . popper-toggle)
  ;;        ("M-`" . popper-cycle)
  ;;        ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-window-height
        (lambda (win)
          (fit-window-to-buffer
           win
           (floor (frame-height) 4)
           (floor (frame-height) 4))))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          "\\*compilation\\*"
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$" shell-mode   ;shell as a popup
          "^\\*term.*\\*$" term-mode     ;term as a popup
          "^\\*vterm.*\\*$" vterm-mode   ;vterm as a popup
          "^\\*HTTP Response\\*$"
          helpful-mode
          magit-process-mode
          "\\*gt-result\\*"
          comint-mode
          eat-mode))
  (setq popper-group-function #'popper-group-by-project)
  (popper-mode 1)
  ;; For echo area hints
  (popper-echo-mode 1))


(provide 'init-window)

;;; init-window.el ends here
