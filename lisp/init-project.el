;; init-project.el --- project configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  project configurations
;;

;;; Code:

;; (use-package counsel-projectile
;;   :hook (after-init . counsel-projectile-mode))

(defvar +project/lsp-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `+project/lsp-project-root`.")

(defun +project/lsp-project-root (&optional dir)
  (let* ((dir (or dir default-directory))
         (cache-key dir)
         (cache-value (gethash cache-key +project/lsp-project-root-cache)))
    (if (and cache-value (file-exists-p cache-value))
        cache-value
      (let* ((lsp-folders (lsp-session-folders (lsp-session)))
             (value (find-if (lambda (path) (string-prefix-p path (expand-file-name dir))) lsp-folders)))
        (puthash cache-key value +project/lsp-project-root-cache)
        value))))

(use-package projectile
  :hook (after-init . projectile-mode)
  :bind ("C-<tab>" . projectile-next-project-buffer)
  :config
  ;; switch project to project root dir instead of project file
  (setq projectile-switch-project-action #'projectile-dired)
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  ;; cache file in ~/.emacs.d/projectile.cache
  (setq projectile-enable-caching t))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-functions #'+project/lsp-project-root)

  (defun +project/projectile-buffer-filter (buffer)
    (let ((name (buffer-name buffer)))
      (or (and (string-prefix-p "*" name)
               (not (string-prefix-p "*eww*" name))
               (not (string-prefix-p "*ein: http" name))
               (not (string-prefix-p "*ein:notebooklist" name))
               (not (string-prefix-p "*vterm:" name)))
          (string-match-p "magit.*:" name)
          (equal (buffer-name (current-buffer)) name))))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/projectile-buffer-filter buffer))
     buffers))

  (setq projectile-buffers-filter-function #'+project/projectile-buffer-filter-function)

  (defun +projectile/ivy-switch-buffer ()
    (interactive)
    (ivy-read "Switch to buffer: "
              (delete (buffer-name (current-buffer))
                      (when (projectile-project-root)
                        (projectile-project-buffer-names)))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+projectile/ivy-switch-buffer))

  ;; FIXME: projectile-project-root is very slow in remote, so I disable it in remote buffer
  ;; (advice-add 'projectile-project-root :before-while
  ;;             (lambda (&optional dir)
  ;;               (let ((dir (or dir default-directory)))
  ;;                 (when (and (fboundp 'tramp-archive-file-name-archive)
  ;;                            (tramp-archive-file-name-p dir))
  ;;                   (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
  ;;                 (not (file-remote-p dir nil t)))))
  )

(use-package find-file-in-project
  :commands (find-file-in-project
             find-file-in-current-directory
             find-file-in-project-not-ignore)
  :config
  (advice-add #'ffip-project-root :around (lambda (orig-fn)
                                            (or (+project/lsp-project-root)
                                                (funcall orig-fn))))

  ;; A simple, fast and user-friendly alternative to 'find'
  ;; https://github.com/sharkdp/fd
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))

  (defun find-file-in-project-not-ignore ()
    (interactive)
    (let ((ffip-rust-fd-respect-ignore-files nil))
      (find-file-in-project))))

;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

(use-package persp-mode
  :defer t)


(provide 'init-project)

;;; init-project.el ends here
