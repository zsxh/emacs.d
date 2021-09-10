;; init-eaf.el --- Emacs Application Framework	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Application Framework
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Install
;;
;; cd ~/.emacs.d
;; git submodule add --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/submodules/emacs-application-framework/
;; cd ~/.emacs.d/submodules/emacs-application-framework
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-browser.git app/eaf-browser
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-pdf-viewer.git app/eaf-pdf-viewer
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-image-viewer.git app/eaf-image-viewer
;;
;; npm install (in each app directory)
;;
;; DEPENDENCIES
;; https://github.com/manateelazycat/emacs-application-framework#install
;; 1) $ sudo pacman -S wmctrl
;; 2) python dependencies:
;; $ yay -S python-pyqt5-webengine
;; $ pip install --user PyMuPDF epc
;; or
;; Install in pyenv independent enviroment
;; $ pip install PyQt5 PyQtWebEngine PyMuPDF epc
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
             eaf-open-browser-with-history)
  :init
  (progn
    (defun eaf-find-file (orig-fn &rest args)
      (require 'eaf)
      (let* ((file (car args))
             (file-extension (file-name-extension file))
             (ext (if file-extension (downcase file-extension) nil)))
        (cond
         ((not ext) (apply orig-fn args))
         ((member (eaf-get-file-name-extension file) eaf-office-extension-list)
          (eaf-open-office file))
         ((eaf--get-app-for-extension
           (eaf-get-file-name-extension file))
          (eaf-open file))
         (t (apply orig-fn args)))))
    (advice-add #'find-file :around #'eaf-find-file))
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-image-viewer)
  (with-eval-after-load 'org
    (require 'eaf-org))

  (add-hook 'eaf-mode-hook
            (lambda ()
              (setq left-fringe-width 0
                    right-fringe-width 0)

              (when (featurep 'evil)
                (setq-local evil-motion-state-map nil)
                ;; "C-z" normal-state -> emacs-state
                ;; ":" evil-ex
                (setq-local evil-normal-state-map
                            '(keymap
                              (58 . evil-ex)
                              (26 . evil-emacs-state))))))
  ;; Remove unwanted advices
  (advice-remove 'find-file #'eaf--find-file-advisor)
  (advice-remove 'dired-find-file #'eaf--dired-find-file-advisor)
  (advice-remove 'dired-find-alternate-file #'eaf--dired-find-file-advisor)

  (setq eaf-python-command "/usr/bin/python3"
        eaf-browser-default-search-engine "duckduckgo"
        eaf-config-location (expand-file-name (locate-user-emacs-file ".cache/eaf/"))
        eaf-proxy-host personal-proxy-http-host
        eaf-proxy-port (format "%s" personal-proxy-http-port)
        eaf-proxy-type "http")

  (defun +eaf/monitor-buffer-kill-a ()
    "A function monitoring when an EAF buffer is killed."
    (run-with-idle-timer
     3 nil
     (lambda (id)
       (ignore-errors
         (eaf-call-async "kill_buffer" id)))
     eaf--buffer-id)
    ;; Kill eaf process when last eaf buffer closed.
    ;; We need add timer to avoid the last web page kill when terminal is exited.
    (run-with-idle-timer
     10 nil
     (lambda ()
       (when (equal (length (eaf--get-eaf-buffers)) 0)
         (eaf--kill-python-process)))))

  (advice-add 'eaf--monitor-buffer-kill :override #'+eaf/monitor-buffer-kill-a))

(use-package eaf-browser
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-browser"
  :defer t
  :config
  (setq eaf-browser-blank-page-url "https://duckduckgo.com"
        eaf-browser-default-zoom 1.2
        eaf-browser-dark-mode nil)
  (eaf-bind-key +eaf/switch-to-eww "C-t" eaf-browser-keybinding)
  (eaf-bind-key nil "M-u" eaf-browser-keybinding)
  (eaf-bind-key clear_focus "M-p" eaf-browser-keybinding)
  (eaf-bind-key nil "T" eaf-browser-keybinding)
  (eaf-bind-key recover_prev_close_page "X" eaf-browser-keybinding)

  (with-eval-after-load 'org
      (setq browse-url-browser-function 'eaf-open-browser))

  ;; overrides
  (defun eaf-open-devtool-page ()
    (split-window (selected-window)
                  (/ (* (nth 3 (eaf-get-window-allocation (selected-window))) 2) 3)
                  nil t)
    (other-window 1)
    (eaf-open "about:blank" "browser" "devtools"))

  (defun +eaf/clear-focus ()
    (interactive)
    (eaf-call-async "call_function" eaf--buffer-id "clear_focus")
    (when (and (bound-and-true-p evil-mode)
               (not (evil-normal-state-p)))
      (evil-normal-state)))

  (add-hook 'eaf-browser-hook
            (lambda ()
              ;; browser toggle insert/normal state except in devtool
              ;; devtool buffer will first open about:blank page and then redirect to devltools:// path
              (unless (string-prefix-p "about:blank" eaf--buffer-url)
                (evil-local-set-key 'insert (kbd "<escape>") #'+eaf/clear-focus)
                (evil-local-set-key 'normal (kbd "<escape>") #'+eaf/clear-focus))))

  (defvar +eaf/evil-focus-timer nil)

  (defun +eaf/evil-focus (&rest _)
    (let ((buf (current-buffer))
          (id eaf--buffer-id))
      (unless (and (bound-and-true-p evil-mode)
                   (evil-insert-state-p))
        (when (timerp +eaf/evil-focus-timer)
          (cancel-timer +eaf/evil-focus-timer))
        (setf +eaf/evil-focus-timer
              (run-with-timer
               0.1
               nil
               (lambda (buf id)
                 (eaf-deferred-chain
                   (eaf-call-async "call_function" id "is_focus")
                   (eaf-deferred-nextc it (lambda (x)
                                            (with-current-buffer buf
                                              ;; (message "debug eaf: is_focus=%s" x)
                                              (if (string= x "True")
                                                  (unless (evil-insert-state-p) (evil-insert-state))
                                                (when (evil-insert-state-p) (evil-normal-state))))))))
               buf
               id)))))
  (advice-add 'eaf--input-message :after #'+eaf/evil-focus)
  (advice-add 'eaf-py-proxy-insert_or_focus_input :after #'+eaf/evil-focus)
  ;; TODO: all insert_or_OPERATIONS
  ;; eaf-py-proxy-insert_or_edit_url

  (defun +eaf/translate-text (text)
    "Use sdcv to translate selected TEXT."
    ;; (message (format "debug:%s" text))
    (if (featurep 'youdao-dictionary)
        (+eaf/youdao-search text)
      (when (package-installed-p 'youdao-dictionary)
        (require 'youdao-dictionary)
        (+eaf/youdao-search text))))

  (defun +eaf/youdao-search (word)
    "Search WORD simple translate result."
    (let ((result (youdao-dictionary--format-result (youdao-dictionary--request word)))
          (posframe-mouse-banish nil)
          (buf-name youdao-dictionary-buffer-name))
      ;; Show tooltip at point if word fetch from user cursor.
      (posframe-show buf-name
                     :string result
                     :position (if (derived-mode-p 'eaf-mode) (+eaf/mouse-absolute-pixel-position) (point))
                     :timeout 5
                     :internal-border-color (face-foreground 'default)
                     :internal-border-width 1)
      (unwind-protect
          (push (read-event " ") unread-command-events)
        (posframe-delete buf-name))))

  (defun +eaf/mouse-absolute-pixel-position ()
    "Neither `mouse-absolute-pixel-position' or `mouse-pixel-position' work for multi monitors eaf frame,
So I do some dirty hacks for my own user case."
    (let* ((p (mouse-absolute-pixel-position))
           (x (car p))
           (y (cdr p)))
      (if (> x 2560)
          (cons (- x 2560) y)
        p)))

  ;; NOTE: press `Ctrl + <mouse-left-click>` to translate
  (advice-add 'eaf-translate-text :override #'+eaf/translate-text))

(use-package eaf-pdf-viewer
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-pdf-viewer"
  :defer t
  :config
  (setq eaf-pdf-dark-mode 'ignore)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-u" eaf-pdf-viewer-keybinding)
  (with-eval-after-load 'evil
    (evil-define-key 'normal eaf-pdf-outline-mode-map (kbd "RET") 'eaf-pdf-outline-jump)))

(use-package eaf-image-viewer
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-image-viewer"
  :defer t
  :config
  (setq eaf-image-extension-list '("jpg" "jpeg" "png" "bmp" "gif" "webp")))

(use-package eaf-org
  :load-path "~/.emacs.d/submodules/emacs-application-framework/extension"
  :defer t
  :config
  (defun +org/eaf-open-file (file-path link-without-schema)
    (eaf-open file-path))
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.gif\\'" . +org/eaf-open-file)
                        ("\\.png\\'" . +org/eaf-open-file)
                        ("\\.jpe?g\\'" . +org/eaf-open-file)
                        ("\\.pdf\\'" . +org/eaf-open-file))))


(provide 'init-eaf)

;;; init-eaf.el ends here
