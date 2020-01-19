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
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
               ,(cons "org" (concat proto "://orgmode.org/elpa/"))))
            ('melpa-mirror
             `(,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))
               ,(cons "org" (concat proto "://orgmode.org/elpa/"))))
            ('emacs-china
             `(,(cons "gnu-cn" (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa-cn" (concat proto "://elpa.emacs-china.org/melpa/"))
               ,(cons "org-cn" (concat proto "://elpa.emacs-china.org/org/"))))
            ('tencent
             `(,(cons "gnu-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))
               ,(cons "org-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/org/"))))
            ('tuna
             `(,(cons "gnu-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
               ,(cons "org-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))
            (archives
             (error "Unknown archives: `%s'" archives))))))

(set-package-archives personal-package-archives)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))

;; Benchmark-init only measures time spent in `require' and `load'
(use-package benchmark-init
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal benchmark-init/tree-mode-map
      "h" 'evil-backward-char
      "gg" 'evil-goto-first-line
      "q" 'quit-window)
    (evil-define-key 'normal benchmark-init/tabulated-mode-map
      "h" 'evil-backward-char
      "gg" 'evil-goto-first-line
      "q" 'quit-window)))

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
  (when (and (version<= "27.1" emacs-version)
             (bound-and-true-p package-quickstart))
    (add-hook 'quelpa-after-hook 'package-quickstart-refresh))
  :config
  ;; To install some packages with quelpa but use use-package-always-ensure to install all others
  ;; from an ELPA repo :ensure needs to be disabled if the :quelpa keyword is found.
  (quelpa-use-package-activate-advice))

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
      (condition-case err
          (mapc (lambda (item)
                  (when (package-installed-p (car (quelpa-arg-rcp item)))
                    (quelpa item)))
                packages-installed-by-quelpa)
        (error
         (message "[Error] +package/quelpa-upgrade, %s" (error-message-string err))
         nil))
      ;; Delete outdate packages
      (dolist (package-info packages-installed-by-quelpa)
        (let ((package (car package-info)))
          (+package/delete-outdate-package package))))))

(defun +package/delete-outdate-package (package)
  "Delete PACKAGE outdate versions."
  (let* ((p-desc-list (cdr (assq package package-alist)))
         (p-desc-cur (car p-desc-list))
         (p-desc-else (cdr p-desc-list))
         (p-desc-to-delete nil))
    (dolist (p-desc p-desc-else)
      (if (version-list-<= (package-desc-version p-desc-cur) (package-desc-version p-desc))
          (progn
            (setf p-desc-to-delete p-desc-cur)
            (setf p-desc-cur p-desc))
        (setf p-desc-to-delete p-desc))
      (package-delete p-desc-to-delete)
      (format "package %s deleted" (package-desc-full-name p-desc-to-delete)))))

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

;; Extensions
(use-package package-utils
  :commands (upgrade-packages upgrade-packages-and-restart upgrade-packages-async)
  :config
  (defun upgrade-packages ()
    (interactive)
    (package-utils-upgrade-all)
    (+package/quelpa-upgrade))

  (defun upgrade-packages-and-restart ()
    (interactive)
    (upgrade-packages)
    (sleep-for 1)
    (restart-emacs))

  (defun upgrade-packages-async ()
    (interactive)
    (if (fboundp 'async-start)
        (progn
          (message "Updating Pakcages ...")
          (async-start
           `(lambda ()
              ,(async-inject-variables "\\`\\(load-path\\)\\'")
              (require 'init-custom)
              (require 'init-package)
              (upgrade-packages)
              (with-current-buffer "*Messages*"
                (buffer-string)))
           (lambda (result)
             (message "%s" result)
             (message "Async Update Done. Restart to complete process."))))
      (message "[Error] upgrade-package-async need async.el installation"))))

;; Multi-file support for `eval-after-load'.
;; Usage:
;; (with-eval-after-load '(and a (or b c))
;;   ...)
(use-package meal
  :quelpa ((meal :fetcher github :repo "twlz0ne/meal.el")))

(use-package async
  :defer t
  :commands (async-start))


(provide 'init-package)

;;; init-package.el ends here
