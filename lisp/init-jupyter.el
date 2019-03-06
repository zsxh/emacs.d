;; init-jupyter.el --- Emacs Jupyter Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; Emacs Jupyter Configurations
;;

;;; Code:

;; An interface ot communicate with Jupyter kernels in Emacs
(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl jupyter-connect-repl))

(with-eval-after-load 'jupyter-repl
  (set-face-foreground 'jupyter-repl-input-prompt "#4F894C")
  (set-face-background 'jupyter-repl-traceback "#FBF8EF"))

(provide 'init-jupyter)

;;; init-jupyter.el ends here
