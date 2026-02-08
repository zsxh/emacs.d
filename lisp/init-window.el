;; init-window.el --- Window Setup	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Window Setup
;;

;;; Code:

;; NOTE: learn `display-buffer-alist'
;; - [popper.el - Popup buffers for Emacs](https://youtu.be/E-xUNlZi3rI?t=948)
;; - [Make an Emacs Buffer Open the Way You Want](https://lambdaland.org/posts/2022-12-27_repl_buffer_on_the_right/)
;; - [Demystifying Emacs’s Window Manager](https://www.masteringemacs.org/article/demystifying-emacs-window-manager)
;; - [The Emacs Window Management Almanac](https://karthinks.com/software/emacs-window-management-almanac/)

(use-package popper
  :bind (("C-`" . popper-toggle-type))
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
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
          (lambda (buf)
            (with-current-buffer buf
              (derived-mode-p 'compilation-mode)))
          "^\\*shell.*\\*$"
          "^\\*eshell.*\\*$"
          term-mode
          "^\\*vterm.*\\*$"
          eat-mode
          "^\\*HTTP Response\\*$"
          helpful-mode
          magit-process-mode
          "\\*gt-result\\*"
          ;; comint-mode
          "*envrc*"))
  (setq popper-group-function #'popper-group-by-project))


(provide 'init-window)

;;; init-window.el ends here
