;; init-ivy.el --- Ivy Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Ivy for Completion
;;

;;; Code:

;; ivy
(use-package ivy
  :bind ((:map ivy-minibuffer-map
               ("C-k" . ivy-previous-line)
               ("C-j" . ivy-next-line)
               ("C-M-j" . ivy-immediate-done)
               ("C-c C-o" . ivy-occur)
               ([escape] . keyboard-escape-quit))
         (:map ivy-switch-buffer-map
               ("C-k" . ivy-previous-line))
         (:map ivy-occur-mode-map
               ("e" . ivy-wgrep-change-to-wgrep-mode)
               ("C-d" . ivy-occur-delete-candidate)
               ("RET" . ivy-occur-press-and-switch))
         (:map ivy-occur-grep-mode-map
               ("e" . ivy-wgrep-change-to-wgrep-mode)
               ("C-d" . ivy-occur-delete-candidate)
               ("RET" . ivy-occur-press-and-switch)))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; use timer to improve the ivy-read performance,
  ;; see https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
  ;; https://github.com/abo-abo/swiper#frequently-asked-questions
  (setq ivy-use-selectable-prompt t)

  (when (fboundp '+ivy/pinyin-config)
    ;; initial input ":" to match pinyin
    (+ivy/pinyin-config)

    (defun ivy--pinyin-regex (str)
      (or (pinyin-to-utf8 str)
          (ivy--regex-plus str)))

    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--pinyin-regex)
            (swiper . ivy--pinyin-regex)
            (counsel-find-file . ivy--pinyin-regex)
            (find-file-in-project . ivy--pinyin-regex)
            (find-file-in-project-by-selected . ivy--pinyin-regex)
            (t . ivy--pinyin-regex))))

  (setq ivy-initial-inputs-alist nil)

  ;; ivy's fuzzy matcher
  ;; (with-eval-after-load 'flx
  ;;   (defun ivy--pinyin-regex-fuzzy (str)
  ;;     (or (pinyin-to-utf8 str)
  ;;         (ivy--regex-fuzzy str)))

  ;;   (setq ivy-re-builders-alist
  ;;         '((ivy-switch-buffer . ivy--regex-plus)
  ;;           (swiper . ivy--pinyin-regex)
  ;;           (t . ivy--pinyin-regex-fuzzy)))

  ;;   ;; no need with initial "^", since using fuzzy
  ;;   (setq ivy-initial-inputs-alist nil))

  (with-eval-after-load 'man
    (cl-pushnew '(Man-completion-table . "^") ivy-initial-inputs-alist))
  (with-eval-after-load 'woman
    (cl-pushnew '(woman . "^") ivy-initial-inputs-alist)))

;; swiper
(use-package swiper
  :commands swiper
  :bind ("C-s" . swiper))

;; counsel
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x))
  :config
  ;; no need with initial "^"
  (setq ivy-initial-inputs-alist nil))

;; TODO: counsel-tramp configs, ssh, docker, ...
;; https://github.com/masasam/emacs-counsel-tramp
;; Tramp ivy interface for ssh server and docker and vagrant
(use-package counsel-tramp
  :commands counsel-tramp)

;; https://github.com/seagle0128/all-the-icons-ivy-rich
(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icon from CANDIDATE in `ivy-rich'."
    (let* ((path (concat ivy--directory candidate))
           (file (file-name-nondirectory path))
           (icon (cond
                  ((file-directory-p path)
                   (all-the-icons-icon-for-dir path nil ""))
                  ((string-match "^/.*:$" path)
                   (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                  ((not (string-empty-p file))
                   (all-the-icons-icon-for-file file :v-adjust -0.05)))))
      (if (symbolp icon)
          (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
        icon)))

  (plist-put ivy-rich-display-transformers-list 'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon (:width 2))
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))))

  (plist-put ivy-rich-display-transformers-list '+project/ivy-switch-buffer
             (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))

  (plist-put ivy-rich-display-transformers-list '+funcs/ivy-switch-major-mode-buffer
             (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))

  (plist-put ivy-rich-display-transformers-list 'counsel-find-file
             '(:columns
               ((ivy-rich-file-icon)
                (ivy-read-file-transformer))
               :delimiter "\t"))

  (plist-put ivy-rich-display-transformers-list 'find-file-in-project
             (plist-get ivy-rich-display-transformers-list 'counsel-find-file))
  ;; FIXME: doesn't work
  ;; (plist-put ivy-rich-display-transformers-list 'find-file-in-project-by-selected
  ;;            (plist-get ivy-rich-display-transformers-list 'counsel-find-file))

  (ivy-rich-mode 1))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs) ; Emacs< 27
  (setq xref-show-definitions-function #'ivy-xref-show-defs) ; Emacs >= 27
  :commands ivy-xref-show-xrefs)

(defun +ivy/pinyin-config ()
  ;; Contribute pengpengxp
  ;; https://emacs-china.org/t/ivy-read/2432/3
  (use-package pinyinlib
    :commands pinyinlib-build-regexp-string)

  (defun my-pinyinlib-build-regexp-string (str)
    (progn
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t)))))

  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ")
           ".*")
          ((equal str "")
           nil)
          (t
           str)))

  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str))
           nil)
          ((equal (substring str 0 1) ":")
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper (split-string
                                                                    (replace-regexp-in-string ":" "" str) "")))
                      ""))
          nil)))


(provide 'init-ivy)

;;; init-ivy.el ends here
