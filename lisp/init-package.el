;; init-package.el --- Emacs Package Managerment	-*- lexical-binding: t -*-

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
;;  Emacs Package Managerment
;;

;;; Code:

(require 'package)

;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(with-eval-after-load 'package
  ;; Dont set custom package-user-dir, doesn't work well with (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; ;; Install into seperate package dirs for each Emacs version, to prevent bytecode incompatibility
  ;; (setq package-user-dir (expand-file-name emacs-version package-user-dir))
  (defun package--save-selected-packages (&optional value)
    "Set and (don't!) save `package-selected-packages' to VALUE."
    (when value
      (setq package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;; Set EPLA
(setq package-archives
      '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("org-cn"   . "http://elpa.emacs-china.org/org/")
        ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

;; Initialize packages
(setq package-enable-at-startup nil)
(when (version< emacs-version "27.0")
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Build and install your Emacs Lisp packages on-the-fly and directly from source
(use-package quelpa-use-package
  :after (use-package)
  :ensure t
  :init
  ;; (setq quelpa-self-upgrade-p nil)
  (setq quelpa-checkout-melpa-p nil)
  :config
  ;; Using quelpa with :ensure
  ;; (setq use-package-ensure-function 'quelpa)
  )


;; Initialization benchmark
(use-package benchmark-init
  :ensure t
  :commands (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate))

;; Extensions
(use-package package-utils
  :ensure t
  :init
  (defalias 'upgrade-packages 'package-utils-upgrade-all)
  (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(provide 'init-package)

;;; init-package.el ends here
