;; init-package.el --- Emacs Package Managerment	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Package Managerment
;;

;;; Code:

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27+
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
             `(,(cons "melpa" (concat proto "://melpa.org/packages/"))
               ,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))))
            ('melpa-mirror
             `(,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))
               ,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))
            ('tencent
             `(,(cons "melpa-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))
               ,(cons "gnu-cn" (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))))
            ('tuna
             ;; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
             `(,(cons "melpa-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
               ,(cons "gnu-cn" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))))
            (archives
             (error "Unknown archives: `%s'" archives))))))

(set-package-archives personal-package-archives)

(setq use-package-always-ensure t
      use-package-verbose t
      use-package-vc-prefer-newest t)

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

(with-eval-after-load 'package
  (setq package-install-upgrade-built-in t
        package-vc-allow-build-commands t)
  (defvar package-upgrade-exclude-vc-pkgs-p t)
  (defvar package-upgrade-exclude-external-status-pkgs-p t)

  ;; may exclude vc pkgs
  (define-advice package--upgradeable-packages (:override (&optional include-builtins) advice)
    ;; Initialize the package system to get the list of package
    ;; symbols for completion.
    (package--archives-initialize)
    (mapcar
     #'car
     (seq-filter
      (lambda (elt)
        (or (let ((available
                   (assq (car elt) package-archive-contents)))
              (and available
                   (or (not package-upgrade-exclude-vc-pkgs-p)
                       (not (package-vc-p (cadr elt))))
                   (or (not package-upgrade-exclude-external-status-pkgs-p)
                       (not (string-equal "external" (package-desc-status (cadr elt)))))
                   (or (and
                        include-builtins
                        (not (package-desc-version (cadr elt))))
                       (version-list-<
                        (package-desc-version (cadr elt))
                        (package-desc-version (cadr available))))))
            (and (not package-upgrade-exclude-vc-pkgs-p)
                 (package-vc-p (cadr elt)))))
      (if include-builtins
          (append package-alist
                  (mapcan
                   (lambda (elt)
                     (when (not (assq (car elt) package-alist))
                       (list (list (car elt) (package--from-builtin elt)))))
                   package--builtins))
        package-alist))))

  ;; upgrade-all should respect `package-install-upgrade-built-in'
  (define-advice package-upgrade-all (:override (&optional query) advice)
    "Refresh package list and upgrade all packages.
If QUERY, ask the user before upgrading packages.  When called
interactively, QUERY is always true.

Currently, packages which are part of the Emacs distribution are
not upgraded by this command.  To enable upgrading such a package
using this command, first upgrade the package to a newer version
from ELPA by either using `\\[package-upgrade]' or
`\\<package-menu-mode-map>\\[package-menu-mark-install]' after `\\[list-packages]'."
    (interactive (list (not noninteractive)))
    (package-refresh-contents)
    (let ((upgradeable (package--upgradeable-packages package-install-upgrade-built-in)))
      (if (not upgradeable)
          (message "No packages to upgrade")
        (when (and query
                   (not (yes-or-no-p
                         (if (length= upgradeable 1)
                             "One package to upgrade.  Do it? "
                           (format "%s packages to upgrade.  Do it?"
                                   (length upgradeable))))))
          (user-error "Upgrade aborted"))
        (mapc #'package-upgrade upgradeable)))))

(when (and (not (file-exists-p package-user-dir))
           (directory-empty-p package-user-dir))
  (package-refresh-contents))


(provide 'init-package)

;;; init-package.el ends here
