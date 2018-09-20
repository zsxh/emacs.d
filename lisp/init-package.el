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
(require 'package)
(setq package-enable-at-startup nil)
(unless package--initialized
  (package-initialize t))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Benchmark-init only measures time spent in `require' and `load'
(use-package benchmark-init
  :ensure t
  :commands (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal benchmark-init/tree-mode-map "q" 'quit-window)
    (evil-define-key 'normal benchmark-init/tabulated-mode-map "q" 'quit-window)))

(defvar emacs-startup-time nil
  "The time it took, in seconds, for Emacs to initialize.")

(defun +package/display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly\
they were loaded at startup.
If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Emacs Loaded %s packages in %.03fs"
           (length package-activated-list)
           (or emacs-startup-time
               (setq emacs-startup-time (float-time (time-subtract (current-time) before-init-time))))))

(add-hook 'emacs-startup-hook #'+package/display-benchmark)

;; Build and install your Emacs Lisp packages on-the-fly and directly from source
(use-package quelpa-use-package
  :after (use-package)
  :ensure t
  :init
  ;; Using quelpa with :ensure
  ;; (setq use-package-ensure-function 'quelpa)
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-checkout-melpa-p nil)
  ;; Avoid loading quelpa unless necessary.
  ;; This improves performance, but can prevent packages from being updated automatically.
  (setq quelpa-use-package-inhibit-loading-quelpa t))

;;;###autoload
(defun +package/quelpa-upgrade ()
  "Upgrade all packages found in `quelpa-cache'.
This provides an easy way to upgrade all the packages for which
the `quelpa' command has been run in the current Emacs session."
  (interactive)
  (unless (featurep 'quelpa)
    (require 'quelpa))
  (when (quelpa-setup-p)
    (let ((quelpa-upgrade-p t))
      (when quelpa-self-upgrade-p
        (quelpa-self-upgrade))
      (setq quelpa-cache
            (cl-remove-if-not #'package-installed-p quelpa-cache :key #'car))
      (setq package-installed-by-quelpa
            (seq-filter (lambda (item) (member ':fetcher item)) quelpa-cache))
      (mapc (lambda (item)
              (when (package-installed-p (car (quelpa-arg-rcp item)))
                (quelpa item)))
            package-installed-by-quelpa))))

;; Extensions
(use-package package-utils
  :ensure t
  :init
  (defalias 'upgrade-packages 'package-utils-upgrade-all)
  (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart)
  :config
  (with-eval-after-load 'quelpa-use-package
    (advice-add #'upgrade-packages :after #'+package/quelpa-upgrade)
    (advice-add #'upgrade-packages-and-restart :override
                (lambda ()
                  (interactive)
                  (upgrade-packages)
                  (sleep-for 1)
                  (restart-emacs)))))


(provide 'init-package)

;;; init-package.el ends here
