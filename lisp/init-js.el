;; init-js.el --- Summary	-*- lexical-binding: t -*-

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
;;  for javascript
;;

;;; Code:

(require 'init-lsp)

(defun +js/lsp-js-config ()
  (+js/set-leader-keys)
  (lsp))

(add-hook 'js-mode-hook '+js/lsp-js-config)
(add-hook 'typescript-mode-hook '+js/lsp-js-config)
(add-hook 'js3-mode-hook '+js/lsp-js-config)
(add-hook 'rjsx-mode-hook '+js/lsp-js-config)

(defun +js/set-leader-keys ()
  (+funcs/set-leader-keys-for-major-mode
   js-mode-map
   "e"  '(nil :which-key "error")
   "en" '(flymake-goto-next-error :which-key "next-error")
   "eN" '(flymake-goto-prev-error :which-key "prev-error")
   "f" '(lsp-format-buffer :which-key "format")
   "g" '(nil :which-key "go")
   "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
   "gr" '(lsp-ui-peek-find-references :which-key "find-references")
   "R" '(lsp-rename :which-key "rename")))

(provide 'init-js)

;;; init-js.el ends here
