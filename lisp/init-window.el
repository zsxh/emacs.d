;; init-window.el --- Window Setup	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Window Setup
;;

;;; Code:

;; TODO: `display-buffer-alist' https://youtu.be/E-xUNlZi3rI?t=948

;; TODO: https://github.com/karthink/popper
;; TODO: https://www.youtube.com/watch?v=E-xUNlZi3rI
(use-package popper
  :defer t
  ;; :bind (("C-`"   . popper-toggle-latest)
  ;;        ("M-`"   . popper-cycle)
  ;;        ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


(provide 'init-window)

;;; init-window.el ends here
