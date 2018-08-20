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

(use-package ess
  :ensure t
  :defer t)

(use-package ess-julia
  :commands ess-julia-mode
  :init (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))

;; FIXME: lsp-julia got problem
;; (use-package lsp-julia
;;   :requires init-lsp
;;   :quelpa ((lsp-julia :fetcher github :repo "non-Jedi/lsp-julia"))
;;   :commands lsp-julia-enable
;;   :hook (ess-julia-mode . lsp-julia-enable))

(use-package julia-mode
  :ensure t
  :defer t)

(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :hook (julia-mode . julia-repl-mode))


(provide 'init-julia)

;;; init-julia.el ends here
