;; init-minibuffer.el --- minibuffer completions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  minibuffer completions
;;

;;; Code:

;; minibuffer ui
(use-package selectrum
  :defer t)

;; similar to Swiper
(use-package consult
  :defer t)

;; filtering `prescient' or `orderless'
(use-package prescient
  :defer t)
(use-package selectrum-prescient
  :defer t)

(use-package orderless
  :defer t
  :config
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  (advice-add 'orderless-regexp :around #'eh-orderless-regexp))

;; minibuffer actions and occur/export features
(use-package embark
  :defer t)

;; Helpful minibuffer annotations, similar to ivy-rich
(use-package marginalia
  :defer t)

;; pinyin support
;;


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
