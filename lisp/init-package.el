;; init-package.el --- Emacs Package Managerment	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

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
    (let ((quelpa-upgrade-p t)
          (packages-installed-by-quelpa nil))
      (when quelpa-self-upgrade-p
        (quelpa-self-upgrade))
      (setq quelpa-cache
            (cl-remove-if-not #'package-installed-p quelpa-cache :key #'car))
      (setq packages-installed-by-quelpa
            (seq-filter (lambda (item) (memq ':fetcher item)) quelpa-cache))
      (mapc (lambda (item)
              (when (package-installed-p (car (quelpa-arg-rcp item)))
                (quelpa item)))
            packages-installed-by-quelpa)
      ;; Delete outdate packages
      (dolist (package-info packages-installed-by-quelpa)
        (let ((package (car package-info)))
          (+package/delete-outdate-package package))))))

(defun +package/delete-outdate-package (package)
  "Delete PACKAGE outdate versions."
  (let* ((p-desc-list (cdr (assq package package-alist)))
         (p-desc-next (car p-desc-list))
         (p-desc-else (cdr p-desc-list))
         (p-desc-to-delete nil))
    (dolist (p-desc p-desc-else)
      (if (version-list-<= (package-desc-version p-desc-next) (package-desc-version p-desc))
          (progn
            (setf p-desc-to-delete p-desc-next)
            (setf p-desc-next p-desc))
        (setf p-desc-to-delete p-desc))
      (package-delete p-desc-to-delete)
      (format "package %s deleted" (package-desc-full-name p-desc-to-delete)))))

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
