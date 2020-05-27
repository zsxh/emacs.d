;;; websocket-client ---   -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "24.4") (websocket "20170829.457"))
;;
;; Original Author
;; https://github.com/felipeochoa/websocket-client
;;
;; I modified it by returning the payload as a utf-8 encoded string
;; https://github.com/zsxh/websocket-client

;;; Commentary:

;;; Code:

(require 'websocket)

(defgroup websocket-client-mode nil
  "WebSockets repl."
  :group 'websocket)

(defface ws-server-header '((t :foreground "MediumBlue"))
  "Face for server message headings."
  :group 'websocket-client-mode)

(defface ws-client-header '((t :foreground "grey50"))
  "Face for client message headings"
  :group 'websocket-client-mode)

(defface ws-client-text '((t :foreground "grey70"))
  "Face for client message headings"
  :group 'websocket-client-mode)

(defvar-local wsc-output-marker nil)
(defvar-local wsc-prompt-marker nil)
(defvar-local wsc-websocket nil)

(defvar websocket-client-extra-headers nil
  "A list of (header-name . header-value) pairs to send with the websocket handshake.")
(defvar-local wsc-custom-headers nil
  "Record the custom headers used when opening a websocket.")

(defvar wsc-ping-interval 15
  "If non-nil, the client will ping the server every WSC-PING-INTERVAL seconds.")
(defvar-local wsc-ping-timer nil
  "The timer object for sending a periodic keepalive ping.")

(defsubst wsc-at-prompt-p ()
  "Return t if point is at the prompt."
  (>= (point) wsc-prompt-marker))

(defun wsc-insert-output (ws &rest text)
  "In buffer for WS, insert TEXT at `wsc-output-marker'."
  (if ws
      (with-current-buffer (process-buffer (websocket-conn ws))
        (save-excursion
          (goto-char wsc-output-marker)
          (apply 'insert text)))
    (save-excursion
      (goto-char wsc-output-marker)
      (apply 'insert text))))

(defun wsc-header (incoming)
  "Return a string header to place above a message.
INCOMING should be t for messages from the server and nil
otherwise."
  (propertize
   (format "[%s - %s]"
           (format-time-string "%Y-%m-%d %H:%M:%S")
           (if incoming "server" "client"))
   'font-lock-face
   (if incoming 'ws-server-header 'ws-client-header)))

(defun wsc-maybe-send ()
  "If point is at the prompt, send the current text to the server."
  (interactive)
  (when (wsc-at-prompt-p)
    (let ((text (buffer-substring-no-properties wsc-prompt-marker (point-max))))
      (wsc-insert-output nil "\n" (wsc-header nil) "\n"
                         (propertize text 'font-lock-face 'ws-client-text))
      (delete-region wsc-prompt-marker (point-max))
      (condition-case nil
          (wsc-send-input text)
        (websocket-closed (wsc-on-close wsc-websocket))))))

(defun wsc-send-input (string)
  "Send STRING to the server."
  (websocket-send-text wsc-websocket string))

(defun wsc-send-ping (&optional buffer)
  "Send a ping message to the server.
BUFFER is the process buffer of the websocket to ping."
  (with-current-buffer (or buffer (current-buffer))
   (condition-case nil
      (websocket-send wsc-websocket (make-websocket-frame :opcode 'ping
                                                          :completep t))
    (websocket-closed nil))))

(defun websocket-client-open (url)
  "Open an interactive buffer to communicate with a websocket at URL."
  (interactive "MWebSocket url: ")
  (let ((buf (get-buffer-create (format "*ws: %s*" url))))
    (pop-to-buffer buf)
    (unless wsc-prompt-marker
      (erase-buffer)
      (websocket-client-mode)
      (insert "\n\n> ")
      (put-text-property (- (point) 2) (point) 'rear-nonsticky '(read-only))
      (put-text-property (- (point) 2) (point) 'font-lock-face 'comint-highlight-prompt)
      (put-text-property (- (point) 2) (point) 'read-only t)
      (setq wsc-output-marker (make-marker))
      (setq wsc-prompt-marker (make-marker))
      (set-marker-insertion-type wsc-output-marker t)
      (set-marker-insertion-type wsc-prompt-marker nil)
      (set-marker wsc-output-marker (point-min))
      (set-marker wsc-prompt-marker (point-max)))
    (setq wsc-websocket (websocket-open url
                                        :on-open (lambda (ws) (wsc-on-open ws buf))
                                        :on-message 'wsc-on-message
                                        :on-close 'wsc-on-close
                                        :on-error 'wsc-on-error
                                        :custom-header-alist websocket-client-extra-headers))
    (setq wsc-custom-headers websocket-client-extra-headers)
    (wsc-init-ws wsc-websocket buf)))

(defvar websocket-client-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'wsc-maybe-send)
    map))

(define-derived-mode websocket-client-mode fundamental-mode "WS"
  "Major mode for sending commands to a websocket server.")

(defun wsc-init-ws (ws buf)
  "Bind WS to BUF."
  (set-process-buffer (websocket-conn ws) buf)
  (with-current-buffer buf (setq wsc-websocket ws)))

(defun wsc-on-open (ws buf)
  "Called when the connection is established for WS.
BUF is the websocket-client buffer for WS."
  (wsc-init-ws ws buf)
  (wsc-insert-output ws (format "\n[%s - connected to %s]"
                                (format-time-string "%Y-%m-%d %H:%M:%S")
                                (websocket-url ws)))
  (setq wsc-ping-timer (run-at-time wsc-ping-interval wsc-ping-interval
                                    'wsc-send-ping (current-buffer))))

(defun wsc-on-message (ws frame)
  "Called when WS receives FRAME."
  (wsc-insert-output ws "\n" (wsc-header t)
                     "\n" (websocket-frame-text frame)))

(defun wsc-on-close (ws)
  "Called when WS is closed by peer."
  (wsc-insert-output ws (format "\n[%s - %s closed connection]"
                                (format-time-string "%Y-%m-%d %H:%M:%S")
                                (websocket-url ws)))
  (with-current-buffer (process-buffer (websocket-conn ws))
    (when wsc-ping-timer
      (cancel-timer wsc-ping-timer)
      (setq wsc-ping-timer nil))))

(defun wsc-on-error (ws type err)
  "Called when WS had a TYPE error ERR."
  (wsc-insert-output ws (propertize "ERROR" 'font-lock-face 'error)
                     (format " in callback `%S': %s" type
                             (websocket-format-error err))))

(provide 'websocket-client)

;;; websocket-client.el ends here
