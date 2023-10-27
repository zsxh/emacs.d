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

(with-eval-after-load 'package
  ;; (setq package-install-upgrade-built-in t)
  (defvar package-upgrade-exclude-vc-pkgs-p t)
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
                   (when package-upgrade-exclude-vc-pkgs-p
                     (not (package-vc-p (cadr elt))))
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
        package-alist)))))


(provide 'init-package)

;;; init-package.el ends here
