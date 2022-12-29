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
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(eval-when-compile
  (setq use-package-always-ensure t
        use-package-verbose t))

;; Benchmark-init only measures time spent in `require' and `load'
(use-package benchmark-init
  :config
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
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

(use-package async
  :defer t
  :commands (async-start))


(provide 'init-package)

;;; init-package.el ends here
