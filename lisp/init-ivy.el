;; init-ivy.el --- Ivy Configuations	-*- lexical-binding: t -*-

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
;;  Ivy for Completion
;;

;;; Code:

;; ivy
(use-package ivy
  :ensure t
  :bind (:map ivy-minibuffer-map
              ("C-k" . ivy-previous-line)
              ("C-j" . ivy-next-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; use timer to improve the ivy-read performance,
  ;; see https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
    ;; ivy's fuzzy matcher
  (with-eval-after-load 'flx
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    ;; no need with initial "^", since using fuzzy
    (setq ivy-initial-inputs-alist nil)))

;; swiper
(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper))

;; counsel
(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

;; ivy-posframe
(use-package ivy-posframe
  :after counsel
  :ensure t
  :config
  (setq ivy-display-function #'ivy-posframe-display)
  (ivy-posframe-enable))

(use-package ivy-xref
  :ensure t
  :commands ivy-xref-show-xrefs
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'init-ivy)

;;; init-ivy.el ends here
