;; init-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

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
;;  Julia Lang Configurations
;;

;;; Code:

(use-package julia-mode
  :ensure t
  :defer t)

;; TODO: julia-mode key bindings
(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :hook (julia-mode . julia-repl-mode))

(require 'init-lsp)

;; TODO: wait until lsp-juia server fully support version 1.0
;; (use-package lsp-julia
;;   :quelpa ((lsp-julia :fetcher github :repo "non-Jedi/lsp-julia"))
;;   :requires init-lsp
;;   :commands lsp-julia-enable
;;   :preface
;;   (defun +julia/lsp-julia-configs ()
;;     (setq-local company-minimum-prefix-length 0)
;;     (setq-local company-backends
;;                 '((company-lsp :separate company-yasnippet)))
;;     (lsp-julia-enable))
;;   :hook (julia-mode . +julia/lsp-julia-configs))


(provide 'init-julia)

;;; init-julia.el ends here
