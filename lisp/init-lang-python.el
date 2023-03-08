;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; NOTE: `PDM' https://chriswarrick.com/blog/2023/01/15/how-to-improve-python-packaging/

;; NOTE: Install: pyright, ruff-lsp
;; pip install --user pyright ruff-lsp --upgrade

;; TODO: pyright custom type stub
;; settings python.analysis.stubPath, https://github.com/microsoft/pyright/blob/main/docs/settings.md
;; customize type stub, https://github.com/microsoft/python-type-stubs
(use-package python
  :ensure nil
  :hook (python-base-mode . eglot-ensure)
  :custom (python-indent-offset 2)
  :config
  ;; TODO: pyright settings `pythonPath'
  (+eglot/set-leader-keys python-mode-map)
  (+eglot/set-leader-keys python-ts-mode-map))

;; Python virtual environment support for Emacs
(use-package pyvenv
  :commands pyvenv-activate)

;; Setup `PYENV_VERSION' environment variable and `python-shell-virtualenv-root' custom variable based on user input
(use-package pyenv-mode
  :commands pyenv-mode)


(provide 'init-lang-python)

;;; init-lang-python.el ends here
