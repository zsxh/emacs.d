;; init-git.el --- Version Control Configuations	-*- lexical-binding: t -*-

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
;;  Version Control Configuations
;;

;;; Code:

(use-package magit
  :commands (magit magit-blame)
  :ensure t
  ;; :hook (magit-blame-mode . (lambda () (setq magit-blame--style
  ;;                                            '(headings (heading-format . "%H %-20a %C %s\n")))))
  :config

  ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
  ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
  ;; (setq auto-revert-check-vc-info t)

  (use-package evil-magit
    :ensure t)
  (evil-define-minor-mode-key 'normal 'with-editor-mode
    ",c" 'with-editor-finish
    ",k" 'with-editor-cancel)
  (evil-define-minor-mode-key 'normal 'magit-blame-mode
    "q" 'magit-blame-quit
    "c" 'magit-blame-cycle-style)
  (use-package magit-todos
    :ensure t
    :hook (magit-mode . magit-todos-mode)
    :config
    (with-eval-after-load 'evil-collection
      (evil-collection-init 'magit-todos))))

(provide 'init-git)

;;; init-git.el ends here
