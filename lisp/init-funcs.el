;; init-funcs.el --- Custom Functions	-*- lexical-binding: t -*-

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
;;  Custom Functions
;;

;;; Code:

(defun zsxh/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall 'text-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun switch-buffer-or-create (name)
  "Switch to the NAME buffer.
If the buffer doesn't exist, create it and write the initial message into it."
  (let* ((scratch-buffer-name name)
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer scratch-buffer)))

(defmacro zsxh/switch-to-buffer-or-create (name)
  "Switch to the NAME buffer.
If the buffer doesn't exist, create it and write the initial message into it.

It returns a lambda function to switch to target buffer."
  (interactive)
  `(lambda () (interactive) (switch-buffer-or-create ,name)))


(provide 'init-funcs)

;;; init-funcs.el ends here
