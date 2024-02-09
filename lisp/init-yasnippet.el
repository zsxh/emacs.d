;; init-yasnippet.el --- yasnippet configs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  yasnippet configs
;;

;;; Code:


;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (("M-/" . yas-expand)
         ("C-c C-y" . yas-insert-snippet))
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  :defer t)


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
