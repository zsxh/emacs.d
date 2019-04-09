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
  :ensure t
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; A better *Help* buffer
(use-package helpful
  :ensure t
  :defer t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . helpful-callable) ;; replace built-in `describe-function'
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable))
  :config
  (with-eval-after-load 'ivy
    (unless (featurep 'flx)
      (dolist (cmd '(helpful-callable
                     helpful-variable
                     helpful-function
                     helpful-macro
                     helpful-command))
        (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map
      "gd" 'evil-goto-definition
      "gg" 'evil-goto-first-line
      "h" 'evil-backward-char
      "q" 'quit-window))

  (when (featurep 'elisp-demos)
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;;;;;;;;;;;; *Buffer* ;;;;;;;;;;;;;;

(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

;;;;;;;;;;;;;; Dired ;;;;;;;;;;;;;;

;; Dired Configs
(use-package dired
  :defer t
  :config
  ;; dired "human-readable" format
  (setq dired-listing-switches "-alh --time-style=long-iso --group-directories-first")
  ;; Customize dired-directory foreground color
  (set-face-foreground 'dired-directory "#3B6EA8")

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

  (+funcs/set-leader-keys-for-major-mode
   dired-mode-map
   "/" '(dired-narrow :which-key "dired-narrow")
   "r" '(dired-narrow-regexp :which-key "dired-narrow-regexp")
   "s" '(+dired/get-size :which-key "get-size")))

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
  :ensure t
  :init
  (defalias '+dired/find-program 'fd-dired)
  :commands fd-dired
  :config
  (setq fd-dired-pre-fd-args "-0 -c never -I"
        fd-dired-ls-option '("| xargs -0 ls -alhdN" . "-ld")))

;; Editable Dired mode configs
(with-eval-after-load 'wdired
  (+funcs/set-leader-keys-for-major-mode
   wdired-mode-map
   "c" '(wdired-finish-edit :which-key "finish edit")
   "k" '(wdired-abort-changes :which-key "abort changes")
   "q" '(wdired-exit :which-key "exit"))
  (with-eval-after-load 'all-the-icons-dired
    (advice-add #'wdired-change-to-wdired-mode :before (lambda () (all-the-icons-dired-mode -1)))
    (advice-add #'wdired-change-to-dired-mode :after (lambda () (all-the-icons-dired-mode)))))

(use-package all-the-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8")
  (use-package font-lock+ :quelpa (font-lock+ :fetcher wiki)))

;;;;;;;;;;;;;;;;;
;; Dired Tools ;;
;;;;;;;;;;;;;;;;;

;; https://github.com/Fuco1/dired-hacks
;; Collection of useful dired additions

;; narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :after dired
  :commands dired-narrow)

;; open files with external applications(just for linux now)
(use-package dired-open
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("C-<return>" . dired-open-xdg)))

;; customizable highlighting for files in dired listings
(use-package dired-rainbow
  :ensure t
  :after dired
  :config
  (when (string-match "--time-style=long-iso" dired-listing-switches)
    (setq dired-rainbow-date-regexp "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]"))
  ;; highlight executable files, but not directories
  (dired-rainbow-define-chmod executable-unix "#4F894C" "-[rw-]+x.*")
  (with-eval-after-load 'all-the-icons-dired
    (setq dired-rainbow-date-regexp (format "%s%s" dired-rainbow-date-regexp "[ ]."))
    (dired-rainbow-define-chmod executable-unix "#4F894C" "-[rw-]+x.*")))

;;;;;;;;;;;;;; Simple HTML Renderer ;;;;;;;;;;;;;;

(use-package shr
  :defer t
  :config
  ;; (setq shr-inhibit-images t)
  ;; Don't use proportional fonts for text
  (setq shr-use-fonts nil))

;; This package adds syntax highlighting support for code block in HTML, rendered by shr.el.
;; The probably most famous user of shr.el is EWW (the Emacs Web Wowser).
(use-package shr-tag-pre-highlight
  :ensure t
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
  :defer t
  :preface
  (defun +eww/toggle-images-display ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))
  :bind (:map eww-mode-map
              ("I" . +eww/toggle-images-display)
              :map eww-link-keymap
              ("I" . +eww/toggle-images-display))
  :hook (eww-mode . (lambda ()
                      (setq-local shr-inhibit-images t)))
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
    (evil-define-key 'normal eww-mode-map
      "b" 'evil-backward-word-begin
      "w" 'evil-forward-word-begin
      "gg" 'evil-goto-first-line
      "gv" '+eww/toggle-images-display
      "G" 'evil-goto-line
      "h" 'evil-backward-char
      "l" 'evil-forward-char
      "v" 'evil-visual-char
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      "&" '+eww/browse-at-point-with-external-browser)

    (evil-define-key 'normal eww-link-keymap "gv" '+eww/toggle-images-display)))


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here
