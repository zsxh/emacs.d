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
             (value (cl-find-if
                     (lambda (path)
                       (and
                        ;; fast filter to improve `ivy-rich-switch-buffer-root' performance, but not accurate
                        (string-prefix-p path (expand-file-name dir))
                        ;; double check if current dir in the lsp-project roots
                        (file-in-directory-p dir path)))
                     lsp-folders)))
        (puthash cache-key value +project/lsp-project-root-cache)
        value))))

(defalias '+project/root 'projectile-project-root)

(defun +project/switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: "
            (delete (buffer-name (current-buffer))
                    (when (+project/root)
                      (projectile-project-buffer-names)))
            :initial-input nil
            :action #'ivy--switch-buffer-action
            :caller '+project/switch-buffer))

(use-package projectile
  :defer 5
  :bind ("C-<tab>" . projectile-next-project-buffer)
  :commands (projectile-switch-project)
  :config
  (projectile-mode)
  ;; switch project to project root dir instead of project file
  (setq projectile-switch-project-action #'projectile-dired)
  (add-to-list 'projectile-project-root-files-bottom-up "pom.xml")
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
               (not (string-prefix-p "*vterm:" name))
               (not (string-prefix-p "*cider" name))
               (not (string-prefix-p "*Python" name)))
          (string-match-p "magit.*:" name)
          (equal (buffer-name (current-buffer)) name))))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/projectile-buffer-filter buffer))
     buffers))

  (setq projectile-buffers-filter-function #'+project/projectile-buffer-filter-function))

(use-package find-file-in-project
  :commands (find-file-in-project
             find-file-in-current-directory
             find-file-in-project-not-ignore
             find-directory-in-project)
  :config
  ;; TODO: improve showing project files on screen immediately and redraw it asynchronously.
  ;; https://github.com/mpereira/.emacs.d/#a-fast-non-projectile-based-project-file-finder
  ;; emacs M-x `info', check minibuffer, process, project
  ;; emacs built-in `completing-read',`completion-table-dynamic'
  (advice-add #'ffip-project-root :around (lambda (orig-fn)
                                            (or (+project/lsp-project-root)
                                                (funcall orig-fn))))
  (add-to-list 'ffip-project-file "pom.xml")

  ;; A simple, fast and user-friendly alternative to 'find'
  ;; https://github.com/sharkdp/fd
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))

  (defun find-file-in-project-not-ignore ()
    (interactive)
    (let ((ffip-rust-fd-respect-ignore-files nil))
      (find-file-in-project)))

  (defun find-directory-in-project (&optional open-another-window)
    (interactive "P")
    (ffip-find-files nil open-another-window t)))

;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

;; TODO: workspace layout
;; https://www.youtube.com/watch?v=HRQhYAz3M-U

(use-package persp-mode
  :defer t)



(provide 'init-project)

;;; init-project.el ends here
