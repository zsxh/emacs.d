;; init-ivy.el --- Ivy Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Ivy for Completion
;;

;;; Code:

;; TODO: https://github.com/raxod502/selectrum#complementary-extensions
;; https://github.com/minad/consult
;; https://github.com/raxod502/prescient.el
;; https://github.com/oantolin/embark/
;; https://github.com/oantolin/orderless

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
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (when IS-WINDOWS
    ;; use timer to improve the ivy-read performance, but it's so aggressive that
    ;; resting your finger on the up/down arrow will suspend all updates to the minibuffer
    ;; see https://github.com/abo-abo/swiper/issues/1218
    (setq ivy-dynamic-exhibit-delay-ms 200))
  ;; ;; https://github.com/abo-abo/swiper#frequently-asked-questions
  (setq ivy-use-selectable-prompt t)

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
    (mapconcat 'my-pinyinlib-build-regexp-string
               (mapcar 'my-pinyin-regexp-helper (split-string str "" t))
               ""))

  (defun ivy--pinyin-regex (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)))

  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--pinyin-regex)
          (swiper . ivy--pinyin-regex)
          ;; (counsel-M-x . ivy--pinyin-regex)
          (counsel-recentf . ivy--pinyin-regex)
          (counsel-find-file . ivy--pinyin-regex)
          (find-file-in-project . ivy--pinyin-regex)
          (find-file-in-project-by-selected . ivy--pinyin-regex)
          (t . ivy--pinyin-regex)))

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

  (plist-put ivy-rich-display-transformers-list '+project/switch-buffer
             (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))

  (plist-put ivy-rich-display-transformers-list '+funcs/switch-major-mode-buffer
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

;; https://github.com/Yevgnen/ivy-rich/issues/87#issuecomment-740440509
;; `ivy-rich-switch-buffer-project' calls `projectile-project-root'(slow) multiple times
;; cache ivy-switch-buffer candidates to speed up
(eval-after-load 'ivy-rich
  (progn
    (defvar +ivy/ivy-rich-cache
      (make-hash-table :test 'equal))

    (defun +ivy/ivy-rich-cache-lookup (delegate candidate)
      (let ((result (gethash candidate +ivy/ivy-rich-cache)))
        (unless result
          (setq result (funcall delegate candidate))
          (puthash candidate result +ivy/ivy-rich-cache))
        result))

    (defun +ivy/ivy-rich-cache-reset ()
      (clrhash +ivy/ivy-rich-cache))

    (defun +ivy/ivy-rich-cache-rebuild ()
      (mapc (lambda (buffer)
              (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
            (buffer-list)))

    (defun +ivy/ivy-rich-cache-rebuild-trigger ()
      (+ivy/ivy-rich-cache-reset)
      (run-with-idle-timer 1 nil '+ivy/ivy-rich-cache-rebuild))

    (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around '+ivy/ivy-rich-cache-lookup)
    (advice-add 'ivy-switch-buffer :after '+ivy/ivy-rich-cache-rebuild-trigger)))

(use-package ivy-posframe
  :after ivy
  :config
  (ivy-posframe-mode)

  (defun +ivy/custom-posframe-poshandler (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (plist-get info :posframe-height))
             3)))

  (defun +ivy/custom-posframe-display (str)
    (ivy-posframe--display str #'+ivy/custom-posframe-poshandler))

  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-display-function-fallback)
          (t . +ivy/custom-posframe-display)))

  (setq ivy-posframe-border-width 5)

  (defun +ivy/posframe-get-fixed-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height 11))
          (width (min (or ivy-posframe-width 200) (round (* 0.62 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))

  (setq ivy-posframe-size-function '+ivy/posframe-get-fixed-size))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs) ; Emacs< 27
  (setq xref-show-definitions-function #'ivy-xref-show-defs) ; Emacs >= 27
  :commands ivy-xref-show-xrefs)

;; sort and filter candidates in Ivy menus
(use-package ivy-prescient
  :after counsel
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-jq)
        ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode))

;; EmacsConf 2020: https://www.youtube.com/watch?v=1SulVSOb3U8
;; Process Json Data
;; https://github.com/200ok-ch/counsel-jq
(use-package counsel-jq
  :commands (counsel-jq))


(provide 'init-ivy)

;;; init-ivy.el ends here
