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
;;   :commands tldr)

(use-package esup
  :defer t)

(use-package ssh-deploy
  :defer t
  :commands (ssh-deploy-hydra))


(provide 'init-experimental)

;;; init-experimental.el ends here
