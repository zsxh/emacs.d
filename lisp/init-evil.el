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
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-insert-state-cursor nil)
  :commands evil-mode
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
               ("M-y" . evil-scroll-line-up)))
  :config
  ;; don't move cursor back, otherwise it will cause problem with lispy/awesome-pair
  (setq evil-move-cursor-back t)
  ;; the cursor is allowed to move past the last character of a line
  (setq evil-move-beyond-eol t)
  ;; show-paren-mode + Evil config
  (setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace normal))

  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; https://emacs.stackexchange.com/questions/31438/possible-not-to-use-undo-tree-in-evil-mode/34214#34214
  ;; https://github.com/emacs-evil/evil/issues/1074
  (setq undo-tree-enable-undo-in-region nil))

;; Evil keybinding collection
(use-package evil-collection
  :after evil
  :config
  ;; (evil-collection-init)
  (with-eval-after-load 'ibuffer
    (evil-collection-init 'ibuffer)
    (evil-define-key 'normal ibuffer-mode-map
      "h" 'evil-backward-char
      "j" 'evil-next-line
      "k" 'evil-previous-line
      "l" 'evil-forward-char
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line))
  (with-eval-after-load 'ediff (evil-collection-init 'ediff))
  (with-eval-after-load 'edebug (evil-collection-init 'edebug))
  (with-eval-after-load 'popup (evil-collection-init 'popup))
  (with-eval-after-load 'arc-mode (evil-collection-init 'arc-mode)))

;; https://github.com/redguardtoo/evil-matchit
;; Usage: '%' go to matching pair
;; TODO: add tags for julia-mode
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

(use-package evil-mark-replace
  :after evil
  :commands (evilmr-replace-in-defun
             evilmr-replace-in-buffer
             evilmr-tag-selected-region
             evilmr-replace-in-tagged-region))

(use-package evil-multiedit
  :after evil
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev))

;; TODO: new keybinding for 'evil-mc-undo-all-cursors, "grq" is hard to remember
(use-package evil-mc
  :hook (after-init . global-evil-mc-mode)
  :config
  ;; "grq" 'evil-mc-undo-all-cursors
  (evil-define-key* '(normal visual) evil-mc-key-map
                    (kbd "M-n") nil
                    (kbd "M-p") nil
                    (kbd "C-n") nil
                    (kbd "C-t") nil
                    (kbd "C-p") nil)
  (add-hook-run-once 'evil-mc-mode-hook
                     (lambda ()
                       (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode)
                       (add-to-list 'evil-mc-incompatible-minor-modes 'awesome-pair-mode))))

(with-eval-after-load 'hydra
  (defhydra hydra-evil-multiedit (:hint nil)
    "evil multiedit"
    ("A" evil-multiedit-match-all "match-all")
    ("n" evil-multiedit-match-and-next "match-and-next")
    ("p" evil-multiedit-match-and-prev "match-and-prev")
    ("t" evil-multiedit-toggle-or-restrict-region "toggle-or-restrict-region")
    ("C-n" evil-multiedit-next "next")
    ("C-p" evil-multiedit-prev "prev")
    ("q" nil "quit"))
  (defhydra hydra-evil-mc (:hint nil)
    "evil-mc"
    ("A" evil-mc-make-all-cursors "match-all")
    ("n" evil-mc-make-and-goto-next-match "match-and-next")
    ("p" evil-mc-make-and-goto-prev-match "match-and-prev")
    ("t" evil-mc-toggle-cursor-on-click "toggle")
    ("C-n" evil-mc-skip-and-goto-next-match "skip-next")
    ("C-p" evil-mc-skip-and-goto-prev-match "skip-prev")
    ("q" evil-mc-undo-all-cursors "quit" :exit t))
  (defhydra hydra-multi-cursors (:hint nil :exit t)
    "
Modes
------------
[_c_] evil-mc (cursor)
[_e_] evil-multiedit (region)"
    ("c" hydra-evil-mc/body)
    ("e" hydra-evil-multiedit/body)))

;; https://github.com/VanLaser/evil-nl-break-undo
;; It means that, for example, after you write an entire paragraph in insert state,
;; and then you hit u in normal state to undo, changes are undone line by line,
;; instead of the whole paragraph disappearing with one swift stroke.
(use-package evil-nl-break-undo
  :after evil
  :hook ((text-mode prog-mode) . evil-nl-break-undo-mode))

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
    (evil-make-overriding-map special-mode-map 'normal))

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
      "RET" 'man-follow
      "gg" 'evil-goto-first-line
      "gj" 'Man-next-section
      "gk" 'Man-previous-section
      "k" 'evil-previous-line
      "m" 'man)))


(provide 'init-evil)

;;; init-evil.el ends here
