;; init-emacs-enhancement.el --- enhance emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  enhance emacs
;;

;;; Code:

;;;;;;;;;;;;;; *Help* ;;;;;;;;;;;;;;

(use-package elisp-demos
  :defer t
  :init
  ;; Tips: bad performance with `company-box' doc request, so only enable elisp-demos for `helpful'
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; A better *Help* buffer
(use-package helpful
  :defer t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . helpful-callable) ;; replace built-in `describe-function'
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
                   helpful-variable
                   helpful-function
                   helpful-macro
                   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map
      "gd" 'evil-goto-definition
      "gg" 'evil-goto-first-line
      "h" 'evil-backward-char
      "q" (lambda nil (interactive) (kill-buffer))))

  (when (featurep 'elisp-demos)
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;;;;;;;;;;;; *Buffer* ;;;;;;;;;;;;;;

(use-package ibuffer-vc
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

(use-package all-the-icons-ibuffer
  :defer t
  :init
  (add-hook-run-once 'ibuffer-hook (lambda () (all-the-icons-ibuffer-mode))))

;;;;;;;;;;;;;; Dired ;;;;;;;;;;;;;;

;; Dired Configs
(use-package dired
  :ensure nil
  :defer t
  :bind (:map dired-mode-map
              ("C-<return>" . 'dired-open-xdg))
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; dired "human-readable" format
        dired-listing-switches "-alh --time-style=long-iso --group-directories-first")

  (defun +dired/get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1))))))

  (defun +dired/dired-jump-a (orig-fn &rest args)
    (let ((buf (current-buffer))
          (mode major-mode))
      (apply orig-fn args)
      (when (eq mode 'dired-mode)
        (kill-buffer buf))))

  (advice-add 'dired-jump :around '+dired/dired-jump-a)

  ;; TODO: 同一个buffer split window 不能删
  (defun +dired/dired-find-file-a (orig-fn &rest args)
    (let ((pre-buf (current-buffer))
          (cur-buf))
      (apply orig-fn args)
      (setq cur-buf (current-buffer))
      (when (and (not (eq pre-buf cur-buf))
                 (eq major-mode 'dired-mode)
                 (null (get-buffer-window cur-buf)))
        (kill-buffer pre-buf))))

  (advice-add 'dired-find-file :around '+dired/dired-find-file-a)

  (with-eval-after-load 'evil-collection
    (evil-collection-init 'dired)
    (evil-define-key 'normal dired-mode-map
      (kbd "SPC") nil
      "," nil
      ":" 'evil-ex
      "F" 'dired-create-empty-file
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      "h" 'evil-backward-char
      "l" 'evil-forward-char
      "v" 'evil-visual-char
      "V" 'evil-visual-line))

  (+funcs/major-mode-leader-keys
   dired-mode-map
   "/" '(dired-narrow :which-key "dired-narrow")
   "r" '(dired-narrow-regexp :which-key "dired-narrow-regexp")
   "s" '(+dired/get-size :which-key "get-size")
   "C" '(dired-ranger-copy :which-key "copy files")
   "P" '(dired-ranger-paste :which-key "paste files")
   "R" '(dired-ranger-move :which-key "move files")
   "T" '(dired-filter-mode :which-key "toggle-dired-filter-mode")
   "a" '(nil :which-key "async/rsync")
   "ac" '(dired-async-do-copy :which-key "dired-async-do-copy")
   "ar" '(dired-rsync :which-key "direc-rsync")))

(defalias '+dired/find-program 'find-name-dired)

(with-eval-after-load 'find-dired
  (setq find-ls-option
        (cons "-print0 | xargs -0 ls -alhdN" "")))

;; install rust fd first: pacman -S fd
;; https://github.com/sharkdp/fd
;; dired find files using 'fd' instead of 'find'
;; https://github.com/yqrashawn/fd-dired
(use-package fd-dired
  :if (executable-find "fd")
  :init
  (defalias '+dired/find-program 'fd-dired)
  :commands fd-dired
  :config
  (setq fd-dired-pre-fd-args "-0 -c never -I"
        fd-dired-ls-option '("| xargs -0 ls -alhdN" . "-ld")))

;; Editable Dired mode configs
(use-package wdired
  :ensure nil
  :defer t
  :config
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t)

  (+funcs/major-mode-leader-keys
   wdired-mode-map
   "/" '(dired-narrow :which-key "dired-narrow")
   "c" '(wdired-finish-edit :which-key "finish edit")
   "k" '(wdired-abort-changes :which-key "abort changes")
   "q" '(wdired-exit :which-key "exit")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xlsx?$" all-the-icons-fileicon "excel" :face all-the-icons-green))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rar$" all-the-icons-octicon "file-zip" :v-adjust 0.0 :face all-the-icons-lmaroon))

  (with-no-warnings
    (advice-add #'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'dired-create-directory :around #'all-the-icons-dired--refresh-advice)
    (advice-add #'wdired-abort-changes :around #'all-the-icons-dired--refresh-advice))

  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons it too many items
      (if (<= (count-lines (point-min) (point-max)) 200)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file
                                                                :face 'all-the-icons-dired-dir-face
                                                                :height 0.9
                                                                :v-adjust all-the-icons-dired-v-adjust)
                                  (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) "  \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
              (forward-line 1)))
        (message "Not display icons because of too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

;;;;;;;;;;;;;;;;;
;; Dired Tools ;;
;;;;;;;;;;;;;;;;;

;; https://github.com/Fuco1/dired-hacks
;; Collection of useful dired additions

;; narrow dired to match filter
(use-package dired-narrow
  :commands dired-narrow)

;; open files with external applications(just for linux now)
(use-package dired-open
  :commands dired-open-xdg)

;; customizable highlighting for files in dired listings
(use-package dired-rainbow
  :after dired
  :config
  (when (string-match "--time-style=long-iso" dired-listing-switches)
    (setq dired-hacks-datetime-regexp "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]"))
  ;; highlight executable files, but not directories
  (dired-rainbow-define-chmod executable-unix "#4F894C" "-[rw-]+x.*"))

(use-package dired-ranger
  :commands (dired-ranger-copy dired-ranger-move dired-ranger-paste))

(use-package dired-filter
  :commands (dired-filter-mode)
  :config
  (setq dired-filter-prefix "/"))

;; https://github.com/stsquad/dired-rsync
;; TODO: customisation `dired-rsync'
(use-package dired-rsync
  :defer t)

;;;;;;;;;;;;;; Simple HTML Renderer ;;;;;;;;;;;;;;

(use-package shr
  :ensure nil
  :defer t
  :config
  ;; (setq shr-inhibit-images t)
  ;; Don't use proportional fonts for text
  (setq shr-use-fonts nil)
  (setq shr-width 75)
  ;; https://github.com/thanhvg/emacs-hnreader/tree/master#recommended-settings-for-eww
  ;; improve pictures srolling
  (defun shr-put-image (spec alt &optional flags)
    "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.
Hack to use `insert-sliced-image' to avoid jerky image scrolling."
    (if (display-graphic-p)
        (let* ((size (cdr (assq 'size flags)))
               (data (if (consp spec)
                         (car spec)
                       spec))
               (content-type (and (consp spec)
                                  (cadr spec)))
               (start (point))
               (image (cond
                       ((eq size 'original)
                        (create-image data nil t :ascent 100
                                      :format content-type))
                       ((eq content-type 'image/svg+xml)
                        (create-image data 'svg t :ascent 100))
                       ((eq size 'full)
                        (ignore-errors
                          (shr-rescale-image data content-type
                                             (plist-get flags :width)
                                             (plist-get flags :height))))
                       (t
                        (ignore-errors
                          (shr-rescale-image data content-type
                                             (plist-get flags :width)
                                             (plist-get flags :height)))))))
          (when image
            (let* ((image-pixel-cons (image-size image t))
                   (image-pixel-width (car image-pixel-cons))
                   (image-pixel-height (cdr image-pixel-cons))
                   (image-scroll-rows (round (/ image-pixel-height (default-font-height)))))
              ;; When inserting big-ish pictures, put them at the
              ;; beginning of the line.
              (when (and (> (current-column) 0)
                         (> (car (image-size image t)) 400))
                (insert "\n"))

              (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
              ;; (if (eq size 'original)
              ;;     (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
              ;;   (insert-image image (or alt "*")))

              (put-text-property start (point) 'image-size size)
              (when (and shr-image-animate
                         (cond ((fboundp 'image-multi-frame-p)
                                ;; Only animate multi-frame things that specify a
                                ;; delay; eg animated gifs as opposed to
                                ;; multi-page tiffs.  FIXME?
                                (cdr (image-multi-frame-p image)))
                               ((fboundp 'image-animated-p)
                                (image-animated-p image))))
                (image-animate image nil 60))))
          image)
      (insert (or alt "")))))

;; This package adds syntax highlighting support for code block in HTML, rendered by shr.el.
;; The probably most famous user of shr.el is EWW (the Emacs Web Wowser).
(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))


;;;;;;;;;;;;;; Emacs Web Wowser ;;;;;;;;;;;;;;

(use-package eww
  :ensure nil
  :defer t
  :preface
  (defun +eww/toggle-images-display ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))
  :bind ((:map eww-mode-map
               ("I" . +eww/toggle-images-display))
         (:map eww-link-keymap
               ("I" . +eww/toggle-images-display)))
  ;; :hook (eww-mode . (lambda ()
  ;;                     (setq-local shr-inhibit-images t)))
  :config
  (defun +eww/browse-at-point-with-external-browser ()
    (interactive)
    (let ((url (eww-suggested-uris)))
      (if (null url)
          (user-error "No link at point")
        (funcall shr-external-browser (if (consp url) (car url) url)))))

  (defun +eww/browse-at-point-with-eaf ()
    (interactive)
    (let ((url (eww-suggested-uris)))
      (eaf-open-url (if (null url)
                        (plist-get eww-data :url)
                      (if (consp url) (car url) url)))))

  (define-key eww-mode-map (kbd "T") '+eww/browse-at-point-with-eaf)
  (define-key eww-mode-map (kbd "&") '+eww/browse-at-point-with-external-browser)

  (with-eval-after-load 'evil
    (with-eval-after-load 'evil-collection
      (evil-collection-init 'eww))

    (setq +eww/scroll-line-jk nil)

    (defun +eww/repeat-until-eww-buffer (orig-fun &rest args)
      (let* ((other-eww-buffers (make-hash-table :test 'eq))
             (eww-buffers (cl-remove-if
                           (lambda (buffer)
                             (not (string-prefix-p "*eww*" (buffer-name buffer))))
                           (buffer-list)))
             (max-iterations (length (buffer-list)))
             (counter 0))
        (dolist (buffer eww-buffers)
          (unless (eq buffer (current-buffer))
            (puthash buffer t other-eww-buffers)))
        (when (cdr-safe eww-buffers)
          (while (and (< counter max-iterations)
                      (not (gethash (current-buffer) other-eww-buffers)))
            (apply orig-fun args)
            (cl-incf counter)))))

    (defun +eww/next-buffer ()
      (interactive)
      (+eww/repeat-until-eww-buffer 'next-buffer))

    (defun +eww/previous-buffer ()
      (interactive)
      (+eww/repeat-until-eww-buffer 'previous-buffer))

    (evil-define-key 'normal eww-mode-map
      "b" 'evil-backward-word-begin
      "w" 'evil-forward-word-begin
      "gg" 'evil-goto-first-line
      "gv" '+eww/toggle-images-display
      "G" 'evil-goto-line
      "h" 'evil-backward-char
      "H" 'eww-back-url
      "J" '+eww/previous-buffer
      "K" '+eww/next-buffer
      "l" 'evil-forward-char
      "L" 'eww-forward-url
      "s" 'eww-switch-to-buffer
      "t" '+eww/toggle-scroll-line-shortcut
      "v" 'evil-visual-char
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      "&" '+eww/browse-at-point-with-external-browser)

    (defun +eww/toggle-scroll-line-shortcut ()
      (interactive)
      (if +eww/scroll-line-jk
          (progn
            (message "Restore now. Press <j>/<k> to next/previous-line ...")
            (evil-define-key 'normal eww-mode-map
              "d" 'eww-download
              "j" 'evil-next-line
              "k" 'evil-previous-line
              "u" 'eww-up-url)
            (setq +eww/scroll-line-jk nil))
        (message "Press <j>/<k> to scroll-line-down/up, <d>/<u> to scroll-down/up")
        (evil-define-key 'normal eww-mode-map
          "d" 'evil-scroll-down
          "j" 'evil-scroll-line-down
          "k" 'evil-scroll-line-up
          "u" 'evil-scroll-up)
        (setq +eww/scroll-line-jk t)))

    (+eww/toggle-scroll-line-shortcut)

    (evil-define-key 'normal eww-link-keymap "gv" '+eww/toggle-images-display)))

;;;;;;;;;;;;;; Setup a menu of recently opened files ;;;;;;;;;;;;;;
(use-package recentf
  :ensure nil
  :defer t
  :config
  (setq recentf-auto-cleanup "05:00am"
        recentf-max-saved-items 200
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))


;;;;;;;;;;;;;; Change priority of minor-mode keymaps ;;;;;;;;;;;;;;
(use-package minor-mode-hack
  :commands show-minor-mode-map-priority)

;;;;;;;;;;;;;; Tree-Sitter ;;;;;;;;;;;;;;
;; https://github.com/ubolonton/emacs-tree-sitter
(use-package tree-sitter
  :if (lambda () (functionp 'module-load))
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

;;;;;;;;;;;;;; insert-char ;;;;;;;;;;;;;;
(use-package insert-char-preview
  :commands insert-char-preview
  :bind ("C-x 8 RET" . insert-char-preview))

;;;;;;;;;;;;;; posframe ;;;;;;;;;;;;;;
(use-package posframe
  :defer t
  :custom
  (posframe-mouse-banish t)
  :config
  (defun posframe--mouse-banish-a (parent-frame &optional posframe)
    "Banish mouse to the (0 . 0) of PARENT-FRAME.
Do not banish mouse when no-accept-focus frame parameter of POSFRAME
is non-nil."
    (when (and posframe-mouse-banish
               ;; Do not banish mouse when posframe can accept focus.
               ;; See posframe-show's accept-focus argument.
               (frame-parameter posframe 'no-accept-focus)
               (not (equal (cdr (mouse-position)) '(10000 . 10000))))
      (set-mouse-position parent-frame 10000 10000)))
  (advice-add 'posframe--mouse-banish :override #'posframe--mouse-banish-a))

;;;;;;;;;;;;;; transient ;;;;;;;;;;;;;;
;; https://github.com/magit/transient/blob/master/docs/transient.org
(use-package transient
  :defer t
  :config
  (setq transient-display-buffer-action '((display-buffer-below-selected)))
  (define-key transient-map (kbd "<escape>") 'transient-quit-one))

;; https://github.com/yanghaoxie/transient-posframe
(use-package transient-posframe
  :after transient
  :config
  (transient-posframe-mode))

;;;;;;;;;;;;;; Garbage-Collection ;;;;;;;;;;;;;;
;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
;;
;; https://gitlab.com/koral/gcmh
;; https://github.com/hlissner/doom-emacs/commit/717d53c6665229a731c55b23f9786c86111b3474
;; https://www.reddit.com/r/emacs/comments/bg85qm/garbage_collector_magic_hack/elniyfv?utm_source=share&utm_medium=web2x
;; https://github.com/hlissner/doom-emacs/issues/3108
;;
;; Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: “My suggestion is
;; to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements
;; in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch.
;; If even a 100-fold increase doesn’t help, there’s some deeper problem with the Lisp code
;; which produces so much garbage, or maybe GC is not the reason for slowdown.”
;; https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
(use-package gcmh
  :init
  (setq garbage-collection-messages t)
  (setq gcmh-idle-delay 15
        ;; gcmh-high-cons-threshold (* 16 1024 1024)
        ;; gcmh-high-cons-threshold #x40000000 ; 1GB
        gcmh-high-cons-threshold (* 128 1024 1024) ; 128MB
        gcmh-verbose t)
  ;; (setq +gcmh/high-cons-threshold gcmh-high-cons-threshold
  ;;       +gcmh/high-cons-threshold-special (* 1024 1024 1024))
  :hook (after-init . gcmh-mode)
  ;; :config
  ;; GC automatically while unfocusing the frame
  ;; `focus-out-hook' is obsolete since 27.1
  ;; (if (boundp 'after-focus-change-function)
  ;;     (add-function :after after-focus-change-function
  ;;                   (lambda ()
  ;;                     (unless (frame-focus-state)
  ;;                       (gcmh-idle-garbage-collect))))
  ;;   (add-hook 'focus-out-hook 'gcmh-idle-garbage-collect))

  ;; ------------------------- minibuffer ---------------------------------
  ;; (defun +gcmh/minibuffer-setup-h ()
  ;;   (setq gcmh-high-cons-threshold +gcmh/high-cons-threshold-special))

  ;; (defun +gcmh/minibuffer-exit-h ()
  ;;   (setq gcmh-high-cons-threshold +gcmh/high-cons-threshold))

  ;; (add-hook 'minibuffer-setup-hook #'+gcmh/minibuffer-setup-h)
  ;; (add-hook 'minibuffer-exit-hook #'+gcmh/minibuffer-exit-h)

  ;; -------------------- buffer local settings ---------------------------
  ;; (defun +gcmh/set-local-high-cons-threshold ()
  ;;   (setq-local gcmh-high-cons-threshold +gcmh/high-cons-threshold-special))

  ;; HACK Org is known to use a lot of unicode symbols (and large org files tend
  ;;      to be especially memory hungry). Compounded with
  ;;      `inhibit-compacting-font-caches' being non-nil, org needs more memory
  ;;      to be performant.
  ;; (with-eval-after-load 'org
  ;;   (add-hook 'org-mode-hook #'+gcmh/set-local-high-cons-threshold))

  ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
  ;;        native JSON library, so we up the GC threshold to stave off
  ;;        GC-induced slowdowns/freezes.
  ;; (with-eval-after-load 'lsp-mode
  ;;   (add-hook 'lsp-mode-hook #'+gcmh/set-local-high-cons-threshold))

  ;; (with-eval-after-load '(and company-box prog-mode)
  ;;   (add-hook 'prog-mode-hook #'+gcmh/set-local-high-cons-threshold))

  ;; (with-eval-after-load 'telega-chat
  ;;   (add-hook 'telega-chat-mode-hook #'+gcmh/set-local-high-cons-threshold))
  )

;;;;;;;;;;;;;; Tramp ;;;;;;;;;;;;;;
;; TODO: Enhance Tramp
;; https://www.eigenbahn.com/2020/01/15/tramp-autologin-insanity
;; https://willschenk.com/articles/2020/tramp_tricks/
;; https://mina86.com/2021/emacs-remote/
(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-cache 120))

;;;;;;;;;;;;;; Long Line Performance Improvement ;;;;;;;;;;;;;;
;; https://www.reddit.com/r/emacs/comments/j2ovcb/comprehensive_guide_on_handling_long_lines_in/g7ag4ds?utm_source=share&utm_medium=web2x&context=3
;; If one enables `global-so-long-mode', long lines will be detected automatically and
;; the remediation action automatically applied. No need for anything else.
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package profiler
  :ensure  nil
  :defer t
  :config
  ;; https://www.murilopereira.com/how-to-open-a-file-in-emacs
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))


;; Calendar
(use-package cal-china-x
  :after calendar
  :config
  (require 'holidays)
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节"))
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays)))

;; TODO: Calfw - A calendar framework for Emacs
;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :defer t
  :config
  (require 'calfw-org))

(use-package calfw-org :defer t)
(use-package calfw-cal :defer t)


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here
