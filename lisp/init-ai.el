;; init-ai.el --- AI packages	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  AI packages
;;

;;; Code:

;; A simple ChatGPT client for Emacs
;; https://github.com/karthink/gptel
;; NOTE: https://github.com/karthink/gptel/wiki#defining-custom-gptel-commands
;; NOTE: https://ollama.com/blog/how-to-prompt-code-llama
(use-package gptel
  :defer t
  :bind (:map gptel-mode-map
         ("C-c h" . gptel-menu))
  :config
  (setq gptel-proxy (format "http://%s:%s" personal-proxy-http-host personal-proxy-http-port)
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-log-level 'debug)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  ;; OpenAI
  (when (bound-and-true-p personal-openai-key)
    (setq gptel-api-key personal-openai-key))
  ;; OpenRouter
  (when (bound-and-true-p personal-openrouter-key)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'personal-openrouter-key
    :models '(anthropic/claude-3.5-sonnet
              anthropic/claude-3.5-sonnet:beta)))
  ;; kimi
  (when (bound-and-true-p personal-kimi-key)
    (gptel-make-openai "Moonshot"
      :key 'personal-kimi-key
      :stream t
      :models '("moonshot-v1-8k"
                "moonshot-v1-32k"
                "moonshot-v1-128k")
      :host "api.moonshot.cn"))
  ;; DeepSeek
  (when (bound-and-true-p personal-deepseek-key)
    ;; default backend
    (setq gptel-backend (gptel-make-openai "DeepSeek"
                          :key 'personal-deepseek-key
                          :stream t
                          :models '("deepseek-chat")
                          :host "api.deepseek.com"))
    ;; defualt model
    (setq gptel-model "deepseek-chat")))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

;; TODO: https://github.com/lanceberge/elysium/tree/main
;; `elysium-query', `elysium-toggle-window'
(use-package elysium
  :defer t)

;; TODO: https://github.com/tninja/aider.el
(use-package aider
  :vc (:url "https://github.com/tninja/aider.el")
  :defer t
  :config
  (setenv "DEEPSEEK_API_KEY" personal-deepseek-key)
  (setenv "OPENROUTER_API_KEY" personal-openrouter-key)
  (setq aider-args
        '("--deepseek"
          "--no-auto-commits"
          "--no-dirty-commits"
          "--no-check-update")))

(transient-define-prefix transient-ai-assistant ()
  ["AI Assistants\n"
   ["Assistants"
    ("ag" "gptel" gptel-menu)
    ("ad" "aider" aider-transient-menu)]
   ["Git"
    ("gc" "gptel-commit" gptel-commit)]])

(global-set-key (kbd "<f8>") #'transient-ai-assistant)


(provide 'init-ai)

;;; init-ai.el ends here
