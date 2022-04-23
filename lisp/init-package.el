;; init-package.el --- Emacs Package Managerment	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Package Managerment
;;

;;; Code:

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))

(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Set EPLA
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository like 'melpa,
'melpa-mirror, 'emacs-china, 'tencent, 'tuna."
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china netease tuna)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu-cn" (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa-cn" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('tencent
             `(,(cons "gnu-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            (archives
             (error "Unknown archives: `%s'" archives))))))

(set-package-archives personal-package-archives)

;; Setup `use-package'
;; how do I specify a specific branch for a git repo?
;; https://github.com/quelpa/quelpa-use-package/issues/8
;; https://github.com/melpa/melpa#recipe-format
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; TIPS:
;; https://stackoverflow.com/questions/46386362/execution-order-of-eval-after-load-vs-hooks-for-a-given-major-mode-in-emacs
;; https://stackoverflow.com/questions/2736087/eval-after-load-vs-mode-hook

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))

;; Benchmark-init only measures time spent in `require' and `load'
;; FIXME: https://github.com/dholm/benchmark-init-el/issues/15
;; (use-package benchmark-init
;;   :if (<= emacs-major-version 27)
;;   :config
;;   (benchmark-init/activate)
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate)
;;   (with-eval-after-load 'evil
;;     (evil-define-key 'normal benchmark-init/tree-mode-map
;;       "h" 'evil-backward-char
;;       "gg" 'evil-goto-first-line
;;       "q" 'quit-window)
;;     (evil-define-key 'normal benchmark-init/tabulated-mode-map
;;       "h" 'evil-backward-char
;;       "gg" 'evil-goto-first-line
;;       "q" 'quit-window)))

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
  :init
  ;; Using quelpa with :ensure
  ;; (setq use-package-ensure-function 'quelpa)
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-melpa-dir (expand-file-name "quelpa/melpa" user-emacs-directory))
  (when (file-exists-p (expand-file-name ".git" quelpa-melpa-dir))
    (setq quelpa-checkout-melpa-p nil))
  ;; Avoid loading quelpa unless necessary.
  ;; This improves performance, but can prevent packages from being updated automatically.
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  (when (bound-and-true-p package-quickstart)
    (add-hook 'quelpa-after-hook 'package-quickstart-refresh))
  :config
  ;; To install some packages with quelpa but use use-package-always-ensure to install all others
  ;; from an ELPA repo :ensure needs to be disabled if the :quelpa keyword is found.
  (quelpa-use-package-activate-advice))

(defun +package/quelpa-clean-cache (pkg-desc &optional force nosave)
  (let ((pkg-name (package-desc-name pkg-desc)))
    (unless (featurep 'quelpa)
      (require 'quelpa))
    (when (and (quelpa-setup-p)
               (not (assq pkg-name package-alist))
               (cl-assoc pkg-name quelpa-cache))
      (let ((build-dir (expand-file-name (symbol-name pkg-name) quelpa-build-dir)))
        (when (file-exists-p build-dir)
          (delete-directory build-dir t))
        (setq quelpa-cache (cl-remove pkg-name quelpa-cache :key #'car))
        (quelpa-save-cache)
        (format "package %s quelpa cache cleaned" pkg-name)))))

(advice-add 'package-delete :after '+package/quelpa-clean-cache)

(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-prompt-before-update t
        auto-package-update-show-preview t
        auto-package-update-hide-results nil
        auto-package-update-excluded-packages nil
        auto-package-update-last-update-day-path (expand-file-name "cache/.last-package-update-day" user-emacs-directory))
  (defalias 'upgrade-packages #'auto-package-update-now)
  :config
  (add-hook 'auto-package-update-before-hook #'package-refresh-contents)

  (defun apu--add-to-old-versions-dirs-list (package)
    "Add package all old version dirs to apu--old-versions-dirs-list"
    (dolist (desc (cddr (assq package package-alist)))
      (add-to-list 'apu--old-versions-dirs-list (package-desc-dir desc))))

  (defun apu--safe-package-install (package)
    (condition-case nil
        (progn
          (let* ((pkg-desc (cadr (assoc package package-archive-contents)))
                 (transaction (package-compute-transaction (list pkg-desc)
                                                           (package-desc-reqs pkg-desc))))
            (package-download-transaction transaction))
          ;; NOTE: Only delete old packages when upgrade successfully
          (when auto-package-update-delete-old-versions
            (apu--add-to-old-versions-dirs-list package))
          (format "%s up to date." (symbol-name package)))
      (error
       (format "Error installing %s" (symbol-name package))))))

;; Multi-file support for `eval-after-load'.
;; Usage:
;; (with-eval-after-load '(and a (or b c))
;;   ...)
(use-package meal
  :quelpa (meal :fetcher github :repo "twlz0ne/meal.el"))

(use-package async
  :defer t
  :commands (async-start))


(provide 'init-package)

;;; init-package.el ends here
