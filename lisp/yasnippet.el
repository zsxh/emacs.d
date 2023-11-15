;; yasnippet.el --- yasnippet configs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  yasnippet configs
;;

;;; Code:


(use-package yasnippet
  :init
  (defun +yasnippet/enable-yas-minor-mode ()
    (cond
     ((eq major-mode 'lisp-interaction-mode)
      (run-with-idle-timer 2 nil (lambda () (yas-minor-mode 1))))
     (t
      (yas-minor-mode 1))))
  :hook ((prog-mode nxml-mode) . +yasnippet/enable-yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (+funcs/major-mode-leader-keys snippet-mode-map
                                 "t" '(yas-tryout-snippet :which-key "yas-tryout-snippet")))


(provide 'yasnippet)

;;; yasnippet.el ends here
