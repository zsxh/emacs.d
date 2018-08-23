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

(defun +funcs/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall 'text-mode)
    (setq buffer-offer-save t)
    $buf))

(defun +funcs/switch-buffer-or-create (name)
  "Switch to the NAME buffer.
If the buffer doesn't exist, create a lisp-interaction buffer
and write the 'initial-scratch-message into it."
  (let* ((target-buffer-name name)
         (target-buffer (get-buffer target-buffer-name)))
    (unless target-buffer
      (setq target-buffer (get-buffer-create target-buffer-name))
      (with-current-buffer target-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer target-buffer)))

(defun +funcs/set-local-key (mode-map args)
  "Define local leader keys with both \"SPC m\" and \",\" once.
Need major-mode-map MODE-MAP and keybidngs map ARGS.

It returns a code string to define local leader keys."
  (defun prefix-m (element)
    (if (stringp element) (format "m%s" element) element))
  (let ((m-args (mapcar #'prefix-m args)))
    `(progn
       (general-define-key
        :states 'normal
        :keymaps ',mode-map
        :major-modes t
        :prefix "SPC"
        "m" '(nil :which-key "major")
        ,@m-args)
       (general-define-key
        :states 'normal
        :keymaps ',mode-map
        :major-modes t
        :prefix ","
        ,@args))))

(defmacro +funcs/define-major-key (m &rest args)
  "Define local leader keys with both \"SPC m\" and \",\" once.
Need major-mode-map(s) M and keybidngs map ARGS.
M can be a atom mode-map or non-empty mode-map list.

It returns a function to define local leader keys."
  (interactive)
  (if (atom m)
      `,@(+funcs/set-local-key m args)
    `(progn
       ,@(mapcar (lambda (x) (+funcs/set-local-key x args)) m))))

(defun +funcs/sudo-edit-current-file ()
  "Sudo edit current file."
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (find-file (concat "/sudo:root@localhost:" (buffer-file-name)))
      (goto-char old-point))))


(provide 'init-funcs)

;;; init-funcs.el ends here
