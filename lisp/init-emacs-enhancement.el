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

;; TODO: support submodules
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
              ("C-<return>" . 'dired-open-xdg)
              ("<tab>" . 'dired-subtree-toggle)
              ("q" . nil))
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; dired "human-readable" format
        dired-listing-switches "-alh --time-style=long-iso --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t)

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

  ;; NOTE: emacs 28 `dired-kill-when-opening-new-dired-buffer'
  (when (version< emacs-version "28")
    (defun +dired/dired-jump-a (orig-fn &rest args)
      "Jump to Dired buffer corresponding to current buffer and
kill the current buffer if it's dired buffer."
      (let ((pre-buf (current-buffer))
            (pre-mode major-mode))
        (apply orig-fn args)
        (when (and (eq pre-mode 'dired-mode)
                   (null (get-buffer-window pre-buf)))
          (kill-buffer pre-buf))))
    (advice-add 'dired-jump :around '+dired/dired-jump-a)

    (defun +dired/dired-find-file-a (orig-fn &rest args)
      (let ((pre-buf (current-buffer))
            (cur-buf))
        (apply orig-fn args)
        (setq cur-buf (current-buffer))
        (when (and (not (eq pre-buf cur-buf))
                   (eq major-mode 'dired-mode)
                   (null (get-buffer-window pre-buf)))
          (kill-buffer pre-buf))))
    (advice-add 'dired-find-file :around '+dired/dired-find-file-a))

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
      "q" nil
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

;; Editable Dired pre-mode configs
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
      ;; NOTE: don't display icons it too many items or remote buffer
      (cond
       ((file-remote-p default-directory)
        (message "Not display icons because of remote buffer."))
       ((> (count-lines (point-min) (point-max)) 200)
        (message "Not display icons because of too many items."))
       (t
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
            (forward-line 1))))))
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

(use-package dired-subtree
  :defer t)

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
  :custom
  (eww-bookmarks-directory (locate-user-emacs-file ".cache/"))
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
      (eaf-open-browser (if (null url)
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
      (kbd "SPC") nil
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
        recentf-save-file (locate-user-emacs-file ".cache/recentf")
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
  :defer 15
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;;;;;;;;;;;;; insert-char ;;;;;;;;;;;;;;
(use-package insert-char-preview
  :commands insert-char-preview
  :bind ("C-x 8 RET" . insert-char-preview))

;;;;;;;;;;;;;; posframe ;;;;;;;;;;;;;;
(use-package posframe
  :defer t
  :config
  (setq posframe-mouse-banish '(10000 . 10000)))

;;;;;;;;;;;;;; transient ;;;;;;;;;;;;;;
;; https://github.com/magit/transient/blob/master/docs/transient.org
(use-package transient
  :defer t
  :custom
  (transient-levels-file (locate-user-emacs-file (convert-standard-filename ".cache/transient/levels.el")))
  (transient-values-file (locate-user-emacs-file (convert-standard-filename ".cache/transient/values.el")))
  (transient-history-file (locate-user-emacs-file (convert-standard-filename ".cache/transient/history.el")))
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
  (setq garbage-collection-messages nil)
  (setq gcmh-idle-delay 15
        gcmh-high-cons-threshold #x40000000 ; 1GB
        gcmh-verbose nil)
  :hook (after-init . gcmh-mode))

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
        remote-file-name-inhibit-cache 120
        tramp-verbose 3))

(use-package tramp-cache
  :ensure nil
  :defer t
  :custom
  (tramp-persistency-file-name (locate-user-emacs-file ".cache/tramp")))

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
  ;; :config
  ;; https://www.murilopereira.com/how-to-open-a-file-in-emacs
  ;; (setf (caar profiler-report-cpu-line-format) 80
  ;;       (caar profiler-report-memory-line-format) 80)
  )


;;;;;;;;;;;;;; Calendar ;;;;;;;;;;;;;;
(use-package calendar
  :ensure nil
  :defer t
  :config
  ;; week starts on Monday
  (setq calendar-week-start-day 1)
  (add-hook-run-once 'calendar-mode-hook (lambda () (require 'cal-china-x))))

(use-package cal-china-x
  :defer t
  :config
  (require 'holidays)
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节"))
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays)))

;; Calfw - A calendar framework for Emacs
;; https://github.com/kiwanami/emacs-calfw
(use-package calfw-cal :defer t)
(use-package calfw-org :commands (cfw:open-org-calendar))
(use-package calfw
  :commands (cfw:open-calendar-buffer)
  :config
  (require 'calfw-org)

  (defun +calfw/auto-refresh-buffer (&rest _args)
    (cfw:refresh-calendar-buffer nil))

  (defun +calfw/setup ()
    (add-hook 'window-configuration-change-hook #'+calfw/auto-refresh-buffer nil 'local))

  (add-hook 'cfw:calendar-mode-hook #'+calfw/setup)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'cfw:calendar-mode 'normal)
    (evil-define-key 'normal cfw:calendar-mode-map
      "h" 'cfw:navi-previous-day-command
      "j" 'cfw:navi-next-week-command
      "k" 'cfw:navi-previous-week-command
      "l" 'cfw:navi-next-day-command
      "q" 'kill-current-buffer
      "<" 'cfw:navi-previous-month-command
      ">" 'cfw:navi-next-month-command
      (kbd "RET") 'cfw:org-open-agenda-day)
    (evil-define-key 'normal cfw:org-schedule-map
      "q" 'kill-current-buffer
      (kbd "RET") 'cfw:org-open-agenda-day)
    (evil-define-key 'normal cfw:org-custom-map
      "q" 'kill-current-buffer
      (kbd "RET") 'cfw:org-open-agenda-day)))

;;;;;;;;;;;;;; Xref ;;;;;;;;;;;;;;
(use-package xref
  :ensure nil
  :bind (("C-M-p" . xref-pop-marker-stack)))

;; https://github.com/jacktasia/dumb-jump#obsolete-commands-and-options
(use-package dumb-jump
  :after xref
  :config
  (setq dump-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;;;;;;;;;;;; jit-lock ;;;;;;;;;;;;;;
(use-package jit-lock
  :ensure nil
  :defer t
  :config
  ;; Increase jit-lock-chunk-size from 500 to 1500 for performance reasons
  ;; https://github.com/emacs-mirror/emacs/commit/400b3c9376a5de033f98476263b7fe65988289a8
  (setq jit-lock-chunk-size 1500))

;;;;;;;;;;;;;; eldoc ;;;;;;;;;;;;;;

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :defer t
  :diminish eldoc-mode)

;;;;;;;;;;;;;; others ;;;;;;;;;;;;;;
(use-package url
  :ensure nil
  :defer t
  :custom
  (url-configuration-directory (locate-user-emacs-file ".cache/url/")))

(use-package bookmark
  :ensure nil
  :defer t
  :custom
  (bookmark-default-file (locate-user-emacs-file ".cache/bookmarks")))

(use-package nsm
  :ensure nil
  :defer t
  :custom
  (nsm-settings-file (locate-user-emacs-file ".cache/network-security.data")))


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here
