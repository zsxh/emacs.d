;; init-exec-path.el --- exec-path configurations	-*- lexical-binding: t -*-

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
;;  exec-path configurations
;;

;;; Code:

(defvar exec-path-from-shell-initialize-p nil)

(use-package exec-path-from-shell
  :defer 1
  :ensure t
  :config
  (defun +env/load-shell-env ()
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "LANG")))
  ;; Initialize
  (+env/load-shell-env)

  (defun exec-path-from-shell-advice ()
    (if exec-path-from-shell-initialize-p
        (message "All shell environment variables already loaded in Emacs!")
      (setq exec-path-from-shell-initialize-p t)
      (+env/load-shell-env)))

  (advice-add #'exec-path-from-shell-initialize :override #'exec-path-from-shell-advice))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
