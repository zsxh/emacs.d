;; init-prog.el --- Common Programing Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Common Programing Settings
;;

;;; Code:

;;;;;;;;;;;;;; Quick Scroll line ;;;;;;;;;;;;;;
;; keymap ("C-l" 'recenter-top-bottom) cycling 25%,top,bottom line position
(add-hook 'prog-mode-hook (lambda () (setq-local recenter-positions '(0.25 top bottom))))

;;;;;;;;;;;;;; DOC ;;;;;;;;;;;;;;

;; require `zeal' installation
;; zeal (dash for linux)
(use-package zeal-at-point
  :commands zeal-at-point)

;; require `zeal' or `dash' docsets
;; helm-dash
(use-package helm-dash
  :commands (helm-dash helm-dash-at-point)
  :bind ((:map helm-map
               ("C-j" . helm-next-line)
               ("C-k" . helm-previous-line)))
  :config
  ;; (setq helm-dash-browser-func 'eww)
  (setq helm-dash-browser-func 'eaf-open-dash)
  (setq helm-dash-docsets-path (expand-file-name "~/.local/share/Zeal/Zeal/docsets"))
  (setq helm-dash-common-docsets (dash-docs-installed-docsets))
  (setq helm-dash-enable-debugging nil))

;;;;;;;;;;;;;; EDIT ;;;;;;;;;;;;;;

;; automatic parenthesis pairing for non prog mode
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :bind ("C-j" . newline-and-indent))

(use-package awesome-pair
  :load-path "~/.emacs.d/submodules/awesome-pair"
  :bind ((:map awesome-pair-mode-map
               ("(" . 'awesome-pair-open-round)
               ("[" . 'awesome-pair-open-bracket)
               ("{" . 'awesome-pair-open-curly)
               (")" . 'awesome-pair-close-round)
               ("]" . 'awesome-pair-close-bracket)
               ("}" . 'awesome-pair-close-curly)
               ("%" . 'awesome-pair-match-paren)
               ("\"" . 'awesome-pair-double-quote)
               ("DEL" . 'awesome-pair-backward-delete)
               ("C-d" . 'awesome-pair-forward-delete)
               ("C-k" . 'awesome-pair-kill)
               ("M-\"" . 'awesome-pair-wrap-double-quote)
               ("M-[" . 'awesome-pair-wrap-bracket)
               ("M-{" . 'awesome-pair-wrap-curly)
               ("M-(" . 'awesome-pair-wrap-round)
               ("M-]" . 'awesome-pair-unwrap)
               ("M-n" . 'awesome-pair-jump-right)
               ("M-p" . 'awesome-pair-jump-left)
               ("M-RET" . 'awesome-pair-jump-out-pair-and-newline)))
  :hook (((prog-mode web-mode conf-mode yaml-mode editorconfig-mode vue-mode) . awesome-pair-mode)
         ((c++-mode java-mode rust-mode) . (lambda () (local-set-key (kbd "<") '+prog/insert-angle)))
         (rust-mode . (lambda () (local-set-key (kbd "|") '+prog/insert-rust-closure))))
  :config
  (defun awesome-pair-in-string-p-advice (&optional state)
    (unless (or (bobp) (eobp))
      (save-excursion
        (or
         (and
          (nth 3 (or state (awesome-pair-current-parse-state)))
          (not (equal (point) (line-end-position))))
         (and
          (eq (get-text-property (point) 'face) 'font-lock-string-face)
          (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
         (and
          (eq (get-text-property (point) 'face) 'font-lock-doc-face)
          (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
         ;; fix single quote pair delete for c/c++/java-mode
         (and
          (eq ?\" (char-syntax (char-before)))
          (eq ?\" (char-syntax (char-after (point)))))))))

  (advice-add 'awesome-pair-in-string-p :override 'awesome-pair-in-string-p-advice)

  (defun +prog/insert-angle ()
    "Insert angle brackets like intellij idea."
    (interactive)
    (save-excursion
      (let ((pos (point))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (if bounds
            (let ((letter (char-after (car bounds))))
              (if (and (eq (upcase letter) letter)
                       (not (eq (downcase letter) letter)))
                  (insert "<>")
                (insert "<")))
          (insert "<"))))
    (forward-char))

  (defun +prog/insert-rust-closure ()
    (interactive)
    (save-excursion
      (if (and (equal major-mode 'rust-mode)
               (eq ?\( (char-before)))
          (insert "||")
        (insert "|")))
    (forward-char))

  (defun +prog/in-empty-pair-p (awesome-in-empty-pair-fn &rest args)
    (or (funcall awesome-in-empty-pair-fn)
        (and (eq ?> (char-after))
             (eq ?< (char-before)))
        (and (equal major-mode 'rust-mode)
             (eq ?| (char-after))
             (eq ?| (char-before)))))

  (advice-add 'awesome-pair-in-empty-pair-p :around '+prog/in-empty-pair-p)

  (with-eval-after-load 'rust-mode
    ;; Reset angle brackets syntax
    (modify-syntax-entry ?< "." rust-mode-syntax-table)
    (modify-syntax-entry ?> "." rust-mode-syntax-table))

  (defun +prog/fix-unbalanced-parentheses-or-forward-char ()
    "Fix missing close pair or just move forward one character."
    (interactive)
    (let ((close (awesome-pair-missing-close)))
      (if close
          (cond ((eq ?\) (matching-paren close))
                 (insert ")"))
                ((eq ?\} (matching-paren close))
                 (insert "}"))
                ((eq ?\] (matching-paren close))
                 (insert "]")))
        (forward-char))))

  (advice-add 'awesome-pair-fix-unbalanced-parentheses :override '+prog/fix-unbalanced-parentheses-or-forward-char)

  (with-eval-after-load 'lispy
    (defun lispy-or-awesome-pair-forward ()
      (interactive)
      (if lispy-mode
          (lispy-forward 1)
        (awesome-pair-jump-right)))

    (defun lispy-or-awesome-pair-backward ()
      (interactive)
      (if lispy-mode
          (lispy-backward 1)
        (awesome-pair-jump-left)))

    (define-key lispy-mode-map (kbd "[") 'awesome-pair-open-bracket)
    (define-key lispy-mode-map (kbd "]") 'awesome-pair-close-bracket)
    (define-key lispy-mode-map (kbd "M-n") 'lispy-or-awesome-pair-forward)
    (define-key lispy-mode-map (kbd "M-p") 'lispy-or-awesome-pair-backward)
    (define-key lispy-mode-map (kbd "M-RET") 'awesome-pair-jump-out-pair-and-newline)))

;; Change variable name style
(use-package string-inflection
  :commands string-inflection-all-cycle)

;;;;;;;;;;;;;; Code Folding ;;;;;;;;;;;;;;
;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :commands hs-minor-mode
  ;; FIXME: https://github.com/millejoh/emacs-ipython-notebook/issues/464#issuecomment-460380151
  ;; the bicycle extension was using hideshow and conflicted with ein
  :hook ((prog-mode) .
         (lambda ()
           (unless (member major-mode '(poly-ein-mode ein:notebook-multilang-mode))
             (hs-minor-mode))))
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "zf") 'hs-hide-level)))

;; https://gitlab.com/jgkamat/rmsbolt
;; RMSBolt tries to make it easy to see what your compiler is doing.
;; It does this by showing you the assembly output of a given source code file.
(use-package rmsbolt
  :commands rmsbolt-mode)

;;;;;;;;;;;;;; Indent ;;;;;;;;;;;;;;
;; Evil shift indent
(defvar prog--indent-variable-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
               idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding\
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

(defun +prog/set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the\
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test prog--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(add-hook 'after-change-major-mode-hook '+prog/set-evil-shift-width)

(setq-default evil-shift-width 2)

(defun +prog/indent-region (numSpaces)
  (let ((regionStart (cond ((use-region-p) (region-beginning))
                           (t (line-beginning-position))))
        (regionEnd (cond ((use-region-p) (region-end))
                           (t (line-end-position)))))
    (save-excursion                  ; restore the position afterwards
      (goto-char regionStart)        ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))    ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)        ; restore the selected region
      )))

(defun +prog/tab-region ()
  (interactive)
  (if (use-region-p)
      (+prog/indent-region 2) ; region was selected, call indent-region
    (insert "  ")              ; else insert four spaces as expected
    ))

(defun +prog/untab-region ()
  (interactive)
  (+prog/indent-region -2))

(defun +prog/hack-tab-key ()
  (interactive)
  ;; (local-set-key (kbd "<tab>") '+prog/tab-region)
  (local-set-key (kbd "<backtab>") '+prog/untab-region))

(dolist (mode '(java-mode))
  (let ((mode-hook (intern (format "%s-hook" mode))))
    (add-hook mode-hook '+prog/hack-tab-key)))


;;;;;;;;;;;;;; Coding styles for multiple developers working on the same project across various editors and IDEs ;;;;;;;;;;;;;;

(use-package editorconfig
  :hook (emacs-startup . editorconfig-mode))

(use-package dumb-jump
  :bind (:map dumb-jump-mode-map
              ("C-M-g" . dumb-jump-go)
              ("C-M-p" . dumb-jump-back)
              ("C-M-q" . dumb-jump-quick-look))
  :hook (prog-mode . dumb-jump-mode)
  :custom (dump-jump-prefer-searcher 'rg))


(provide 'init-prog)

;;; init-prog.el ends here
