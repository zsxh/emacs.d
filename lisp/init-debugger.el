;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

;; [Dape] Debug Adapter Protocol for Emacs
;; NOTE: https://github.com/svaante/dape#supported-debug-adapters
(use-package dape
  :vc (:url "https://github.com/svaante/dape" :rev :newest)
  :after eglot)

(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode python-mode)
                 command "python3"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default)))



(provide 'init-debugger)

;;; init-debugger.el ends here
