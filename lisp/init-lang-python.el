;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; NOTE: `PDM' https://chriswarrick.com/blog/2023/01/15/how-to-improve-python-packaging/
;; > pipx install "pdm[all]"
;; NOTE: python linter, formatter
;; > pipx install ruff
;; > pipx install black

;; Python virtual environment support for Emacs
(use-package pyvenv
  :commands pyvenv-activate)

;; NOTE: install python language server
;; > pipx install basedpyright
(use-package python
  :ensure nil
  :hook (python-base-mode . eglot-ensure)
  :custom (python-indent-offset 2))

;; python ruff linter
(use-package flymake-ruff
  :defer t
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p 'python-base-mode)
                                  (flymake-ruff-load)))))

(with-eval-after-load 'python
  ;; formatter
  (require 'reformatter)
  (reformatter-define black-format
    :program "black"
    :args '("-q" "-"))
  (reformatter-define ruff-format
    :program "ruff"
    :args '("--fix-only" "-"))
  (defun +python/format-buffer ()
    (interactive)
    (ruff-format-buffer)
    (black-format-buffer))

  ;; keybindings
  (+eglot/set-leader-keys python-mode-map)
  (+eglot/set-leader-keys python-ts-mode-map)
  (+funcs/major-mode-leader-keys
   python-mode-map
   "f" '(+python/format-buffer :which-key "format")
   "v" '(nil :which-key "venv")
   "va" '(pyvenv-activate :which-key "activate")
   "vd" '(pyvenv-deactivate :which-key "deactivate"))
  (+funcs/major-mode-leader-keys
   python-ts-mode-map
   "f" '(+python/format-buffer :which-key "format")
   "v" '(nil :which-key "venv")
   "va" '(pyvenv-activate :which-key "activate")
   "vd" '(pyvenv-deactivate :which-key "deactivate"))

  ;; TODO: Check https://github.com/wyuenho/emacs-pet
  ;; to support .pdm-python file
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
