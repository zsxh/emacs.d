;; init-experimental.el --- Experimental Feature	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Experimental Feature
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Use system-wide command instead now
;; Simplified and community-driven man pages
;; (use-package tldr
;;   :ensure t
;;   :commands tldr)

(use-package esup
  :ensure t
  :defer t)


(provide 'init-experimental)

;;; init-experimental.el ends here
