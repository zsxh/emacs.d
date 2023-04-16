;; init-package.el --- Emacs Package Managerment	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Package Managerment
;;

;;; Code:

;; NOTE: `package-vc-install', `package-vc-update', `package-vc-update-all' for packages not in mepla/elpa

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

;; https://tony-zorman.com/posts/2022-11-30-package-vc-install.html
;; NOTE: https://github.com/slotThe/vc-use-package
(defconst slot/package-vc-fetchers
  '(:github "https://github.com/"
    :gitlab "https://gitlab.com/"
    :codeberg "https://codeberg.org/"
    :sourcehut "https://git.sr.ht/~")
  "Places from where to fetch packages.")

(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((fetcher-url (plist-get slot/package-vc-fetchers (intern (concat ":" fetcher))))
         (url (format "%s%s" fetcher-url repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url rev backend iname))))

(eval-when-compile
  (setq use-package-always-ensure t
        use-package-verbose t))

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

;; Commands: `auto-package-update-now', `auto-package-update-now-async'
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-prompt-before-update t
        auto-package-update-show-preview t
        auto-package-update-hide-results nil
        auto-package-update-excluded-packages nil
        auto-package-update-excluded-packages
        '(elispfl org-block-capf screenshot)
        auto-package-update-last-update-day-path (expand-file-name "cache/.last-package-update-day" user-emacs-directory))
  :defer t
  :config
  (add-hook 'auto-package-update-before-hook #'package-refresh-contents)

  (defun apu--add-to-old-versions-dirs-list (package)
    "Add package all old version dirs to apu--old-versions-dirs-list"
    (dolist (desc (cddr (assq package package-alist)))
      (add-to-list 'apu--old-versions-dirs-list (package-desc-dir desc))))

  (defun apu--safe-package-install (package)
    (condition-case nil
        (when-let* ((info (assoc package package-archive-contents))
                    (pkg-desc (cadr info))
                    (transaction (package-compute-transaction (list pkg-desc)
                                                              (package-desc-reqs pkg-desc))))
          (package-download-transaction transaction)
          ;; NOTE: Only delete old packages when upgrade successfully
          (when auto-package-update-delete-old-versions
            (apu--add-to-old-versions-dirs-list package))
          (format "%s up to date." (symbol-name package)))
      (error
       (format "Error installing %s" (symbol-name package))))))


(provide 'init-package)

;;; init-package.el ends here
