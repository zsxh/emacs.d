;; init-diagnostic.el --- Diagnostic Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Diagnostic Configuations
;;

;;; Code:

;;;;;;;;;;;;;; FLYMAKE ;;;;;;;;;;;;;;

(use-package flymake
  :defer t
  :ensure nil
  :config
  (setq flymake-no-changes-timeout 2
        flymake-start-on-save-buffer nil)
  (advice-add 'flymake-eldoc-function :override #'ignore))

;; TODO: flymake-collection
;; https://github.com/mohkale/flymake-collection

(use-package popon
  :init (slot/vc-install :fetcher "codeberg" :name "popon" :repo "akib/emacs-popon")
  :ensure nil
  :defer t)

;; https://codeberg.org/akib/emacs-flymake-popon/src/branch/master
(use-package flymake-popon
  :init (slot/vc-install :fetcher "codeberg" :name "flymake-popon" :repo "akib/emacs-flymake-popon")
  :ensure nil
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method 'posframe
        flymake-popon-width 80
        flymake-popon-delay 0.3))


(provide 'init-diagnostic)

;;; init-diagnostic.el ends here
