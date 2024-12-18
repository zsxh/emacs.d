;; init-evil.el --- Evil Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Evil Configuations
;;

;;; Code:

(global-set-key (kbd "C-a") '+funcs/smart-beginning-of-line)

;; Vim edit style
;; tips: press "%" in normal mode to jump between code block, tags
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-insert-state-cursor nil
        evil-esc-delay 0.001
        evil-undo-system 'undo-redo
        evil-respect-visual-line-mode t)
  :hook (after-init . evil-mode)
  :bind ((:map evil-normal-state-map
               ("C-a" . +funcs/smart-beginning-of-line)
               ("C-e" . move-end-of-line)
               ("C-n" . next-line)
               ("C-p" . previous-line)
               ;; Do not C-u to `evil-scroll-up' since C-u in emacs is `universal-argument' prefix key.
               ;; Use C-b `evil-scroll-page-up' instead
               ;; ("C-d" . evil-scroll-down)
               ;; ("C-u" . evil-scroll-up)
               ("C-b" . 'evil-scroll-page-up)
               ("C-f" . 'evil-scroll-page-down)
               ("C-y" . evil-paste-before)
               ("M-e" . evil-scroll-line-down)
               ("M-y" . evil-scroll-line-up))
         (:map evil-motion-state-map
               ([remap evil-goto-definition] . xref-find-definitions)
               ("C-a" . +funcs/smart-beginning-of-line)
               ("C-e" . move-end-of-line)
               ("C-n" . next-line)
               ("C-p" . previous-line)
               ;; ("C-d" . evil-scroll-down)
               ;; ("C-u" . evil-scroll-up)
               ("C-b" . 'evil-scroll-page-up)
               ("C-f" . 'evil-scroll-page-down)
               ("C-y" . evil-paste-before)
               ("M-e" . evil-scroll-line-down)
               ("M-y" . evil-scroll-line-up)
               (";" . evil-repeat-find-char)
               ;; We already use "," as major mode leader key, so use "C-;" here
               ("C-;" . evil-repeat-find-char-reverse)))
  :config
  (setq evil-echo-state nil
        ;; don't move cursor back, otherwise it will cause problem with lispy/awesome-pair
        evil-move-cursor-back nil
        ;; the cursor is allowed to move past the last character of a line
        evil-move-beyond-eol t
        ;; show-paren-mode + Evil config
        evil-highlight-closing-paren-at-point-states '(not emacs insert replace normal)
        evil-want-fine-undo t
        evil-ex-interactive-search-highlight 'selected-window
        evil-cross-lines t
        evil-kill-on-visual-paste nil
        evil-shift-width 2)

  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

;; Evil keybinding collection
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
  ;; (evil-collection-init)
  (with-eval-after-load 'ibuffer
    (evil-collection-ibuffer-setup)
    (evil-define-key 'normal ibuffer-mode-map
      "h" 'evil-backward-char
      "j" 'evil-next-line
      "k" 'evil-previous-line
      "l" 'evil-forward-char
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line))

  (with-eval-after-load 'ediff (evil-collection-ediff-setup))
  (with-eval-after-load 'edebug (evil-collection-edebug-setup))
  (with-eval-after-load 'popup (evil-collection-popup-setup))
  (with-eval-after-load 'calendar (evil-collection-calendar-setup))
  (with-eval-after-load 'info
    (evil-collection-info-setup)
    (evil-define-key 'normal Info-mode-map
      "b" 'evil-backward-word-begin
      "e" 'evil-forward-word-end
      "h" 'evil-backward-char
      "H" 'Info-history-back
      "l" 'evil-forward-char
      "q" 'Info-history-back
      "w" 'evil-forward-word-begin))
  (with-eval-after-load 'arc-mode
    (evil-collection-arc-mode-setup)
    (evil-define-key 'normal archive-mode-map
      (kbd "C-d") 'evil-scroll-down))
  (with-eval-after-load 'cus-edit (evil-collection-custom-setup))
  (with-eval-after-load 'xwidget (evil-collection-xwidget-setup))
  (with-eval-after-load 'ert (evil-collection-ert-setup)))

;; https://github.com/redguardtoo/evil-matchit
;; Usage: '%' go to matching pair
(use-package evil-matchit
  :after evil
  :init
  (define-key evil-motion-state-map (kbd "%") 'evilmi-jump-items)
  :commands (evilmi-jump-items)
  :config
  (global-evil-matchit-mode 1))

;; https://github.com/emacs-evil/evil-surround#examples
;; google "vim text object" to learn more
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(with-eval-after-load 'evil
  ;; evil key bindings for some emacs built-in packages
  (define-key special-mode-map "h" nil)

  ;; package-menu-mode-map have higher priority than evil key bingdings
  (with-eval-after-load 'package
    (evil-set-initial-state 'package-menu-mode 'normal)
    (evil-make-overriding-map package-menu-mode-map 'normal)
    ;; (add-hook 'package-menu-mode-hook #'evil-normalize-keymaps)
    )

  ;; message-buffer-mode evil key bindings
  (evil-define-key 'normal messages-buffer-mode-map
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    "q" 'quit-window)

  ;; compilation-mode evil key bindings
  (with-eval-after-load 'compile
    (evil-set-initial-state 'compilation-mode 'normal)
    (evil-define-key 'normal compilation-mode-map
      "h" 'evil-backward-char
      "gg" 'evil-goto-first-line
      "gr" 'recompile))

  ;; evil keybindings for simple package
  (with-eval-after-load 'simple
    (evil-set-initial-state 'process-menu-mode 'normal)
    (evil-make-overriding-map process-menu-mode-map 'normal)

    (evil-set-initial-state 'special-mode 'normal)
    (evil-make-overriding-map special-mode-map 'normal)
    (evil-define-key 'normal special-mode-map
      "gg" 'evil-goto-first-line
      "gr" 'revert-buffer))

  (with-eval-after-load 'imenu-list
    (evil-define-key 'normal imenu-list-major-mode-map
      "d" 'imenu-list-display-entry
      "gg" 'evil-goto-first-line
      "gr" 'imenu-list-refresh))

  (with-eval-after-load 'debug
    (evil-set-initial-state 'debugger-mode 'normal)
    (evil-define-key 'normal debugger-mode-map
      (kbd "<return>") 'backtrace-help-follow-symbol
      "h" 'evil-backward-char
      "j" 'evil-next-line
      "J" 'debugger-jump
      "?" 'describe-mode
      "l" 'evil-forward-char
      "L" 'debugger-list-functions))

  (with-eval-after-load 'profiler
    (evil-define-key 'normal profiler-report-mode-map
      "h" 'evil-backward-char
      "j" 'evil-next-line
      "k" 'evil-previous-line
      "l" 'evil-forward-char
      "?" 'describe-mode))

  (with-eval-after-load 'man
    (evil-set-initial-state 'Man-mode 'normal)
    (evil-define-key 'normal Man-mode-map
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "RET") 'man-follow
      "gg" 'evil-goto-first-line
      "gj" 'Man-next-section
      "gk" 'Man-previous-section
      "k" 'evil-previous-line
      "m" 'man))

  ;; (with-eval-after-load 'comint
  ;;   (evil-set-initial-state 'comint-mode 'normal))

  ;; Customize

  ;; Code faster by extending Emacs EVIL text object
  ;; http://blog.binchen.org/posts/code-faster-by-extending-emacs-evil-text-object/
  ;; press `vib' to do exactly same thing as vi(
  ;; using `vig' to replace vi[, vi{, vi(, and vi<
  (defun my-evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
    (let* ((parens '("()" "[]" "{}" "<>"))
           range
           found-range)
      (dolist (p parens)
        (condition-case nil
            (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
          (error nil))
        (when range
          (cond
           (found-range
            (when (< (- (nth 1 range) (nth 0 range))
                     (- (nth 1 found-range) (nth 0 found-range)))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))

  (evil-define-text-object my-evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (my-evil-paren-range count beg end type t))

  (evil-define-text-object my-evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (my-evil-paren-range count beg end type nil))

  (define-key evil-inner-text-objects-map "g" #'my-evil-inner-paren)
  (define-key evil-outer-text-objects-map "g" #'my-evil-a-paren))


(provide 'init-evil)

;;; init-evil.el ends here
