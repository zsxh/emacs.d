;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; NOTE: `PDM' https://chriswarrick.com/blog/2023/01/15/how-to-improve-python-packaging/

;; Python virtual environment support for Emacs
(use-package pyvenv
  :commands pyvenv-activate)

;; Setup `PYENV_VERSION' environment variable and `python-shell-virtualenv-root' custom variable based on user input
(use-package pyenv-mode
  :commands pyenv-mode)

;; NOTE: Install: pyright
;; $ npm install -g pyright
(use-package python
  :ensure nil
  :hook (python-base-mode . eglot-ensure)
  :custom (python-indent-offset 2)
  :config
  (+eglot/set-leader-keys python-mode-map)
  (+eglot/set-leader-keys python-ts-mode-map))

(with-eval-after-load 'python
  ;; TODO: Check https://github.com/wyuenho/emacs-pet
  (defun +python/locate-venv-python-cmd ()
    "Look for virtual environments local to the workspace."
    (when-let* ((project-dir (+project/root))
                (venv-dir (or
                           (when-let ((venv (locate-dominating-file project-dir "venv")))
                             (file-name-concat venv "venv"))
                           (when-let ((venv (locate-dominating-file project-dir ".venv")))
                             (file-name-concat venv ".venv"))))
                (python-cmd (executable-find (file-name-concat venv-dir "bin" "python"))))
      python-cmd))

  (require 'eglot)

  ;; https://microsoft.github.io/pyright/#/settings
  (defun +python/workspace-configuration (&optional server)
    (if-let ((venv-python-cmd (+python/locate-venv-python-cmd)))
        `(:python
          (:pythonPath ,venv-python-cmd))))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode python-mode))
    (+python/workspace-configuration))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode python-ts-mode))
    (+python/workspace-configuration)))


(provide 'init-lang-python)

;;; init-lang-python.el ends here
