;; init-python.el --- python configurations	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

(use-package pyenv-mode
  :ensure t
  :hook (python-mode . pyenv-mode)
  )

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended)
  :config
  (require 'init-lsp)
  (use-package lsp-python
    ;; :requires init-lsp
    :ensure t
    :config
    (lsp-python-enable)))

;; Extra Keybindings
(with-eval-after-load 'lsp-python
  (require 'general)
  (zsxh/define-major-key python-mode-map
                         "g"  '(nil :which-key "go")
                         "gg" '(xref-find-definitions :which-key "goto-definition")
                         "gr" '(xref-find-references :which-key "find-references")
                         "r"  '(nil :which-key "refactor")
                         "rr" '(lsp-rename :which-key "rename")))


(provide 'init-python)

;;; init-python.el ends here
