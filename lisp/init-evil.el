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
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-insert-state-cursor nil)
  :commands evil-mode
  :hook (after-init . evil-mode)
  :bind (:map
         evil-normal-state-map
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("C-d" . evil-scroll-down)
         ("C-u" . evil-scroll-up)
         ("C-a" . +funcs/smart-beginning-of-line)
         ("C-e" . move-end-of-line)
         ("C-y" . evil-paste-before)
         ("M-e" . evil-scroll-line-down)
         ("M-y" . evil-scroll-line-up)
         :map evil-motion-state-map
         ("C-e" . move-end-of-line))
  :config
  ;; don't move cursor back, otherwise it will cause problem with lispy/awesome-pair
  (setq evil-move-cursor-back nil)

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
  :ensure t
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
  (with-eval-after-load 'edebug (evil-collection-init 'edebug)))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; https://github.com/VanLaser/evil-nl-break-undo
;; It means that, for example, after you write an entire paragraph in insert state,
;; and then you hit u in normal state to undo, changes are undone line by line,
;; instead of the whole paragraph disappearing with one swift stroke.
(use-package evil-nl-break-undo
  :after evil
  :ensure t
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
      "?" 'describe-mode)))


(provide 'init-evil)

;;; init-evil.el ends here
