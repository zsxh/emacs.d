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

;; NOTE: Install: pyright, ruff-lsp
;; pip install --user pyright ruff-lsp --upgrade

;; TODO: pyright custom type stub
;; settings python.analysis.stubPath, https://github.com/microsoft/pyright/blob/main/docs/settings.md
;; customize type stub, https://github.com/microsoft/python-type-stubs
(use-package python
  :ensure nil
  :hook (python-base-mode . +python/enable-lsp)
  :custom (python-indent-offset 2)
  :config
  (+eglot/set-leader-keys python-mode-map)
  (+eglot/set-leader-keys python-ts-mode-map))

(defun +python/enable-lsp ()
  (setq eglot-workspace-configuration #'+python/workspace-configuration)
  (eglot-ensure))

(defun +python/workspace-configuration (&optional server)
  (when-let* ((config-file (file-name-concat user-emacs-directory "lsp-config" "pyright.json"))
              (settings (with-temp-buffer
                          (insert-file-contents config-file)
                          (json-parse-buffer :object-type 'plist))))
    (if-let ((venv-python-cmd (+python/locate-venv-python-cmd))
             (python-section (plist-get settings :python)))
        (plist-put python-section :pythonPath venv-python-cmd))
    settings))

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



(provide 'init-lang-python)

;;; init-lang-python.el ends here
