;; init-common-lisp.el --- Common Lisp Configurations	-*- lexical-binding: t -*-

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
;;  Common Lisp Configurations
;;

;;; Code:

(use-package sly
  :ensure t
  :hook ((lisp-mode . sly-mode)
         (sly-mode . +common-lisp|init-sly))
  :config
  (setq inferior-lisp-program "sbcl")
  (evil-set-initial-state 'sly-db-mode 'emacs))

(use-package sly-macrostep
  :ensure t
  :commands macrostep-expand
  :bind (:map lisp-mode-map
              ("C-c e" . macrostep-expand)))

(defun +common-lisp|init-sly ()
  "Attempt to auto-start sly when opening a Lisp buffer."
  (cond ((sly-connected-p))
        ((executable-find inferior-lisp-program)
         (let ((sly-auto-start 'always))
           (sly-auto-start)
           (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
        ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                  inferior-lisp-program))))

(defun +common-lisp|cleanup-sly-maybe ()
  "Kill processes and leftover buffers when killing the last sly buffer."
  (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                   if (and (buffer-local-value 'sly-mode buf)
                           (get-buffer-window buf))
                   return t)
    (dolist (conn (sly--purge-connections))
      (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (mapc #'kill-buffer
            (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (buffer-local-value 'sly-mode buf)
                     collect buf)))))

;; TODO quicklisp


(provide 'init-common-lisp)

;;; init-common-lisp.el ends here
