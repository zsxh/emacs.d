# emacs.d

A growing collection of **prompt templates** for **GPTel** (the built‑in Emacs LLM interface).  
These snippets cover everything from code review and refactoring to business strategy, deep‑dive analysis, and creative storytelling—ready to drop into your Emacs workflow.

---

## 📖 What’s Inside?

| Prompt file | Theme / Purpose | Quick note |
|------------|----------------|------------|
| **ask‑ai** | General‑purpose AI prompting with persuasion and thoughtful perspective | Extend user input, challenge assumptions |
| **code‑explain** | Clear, step‑by‑step explanations of code snippets | Beginner‑friendly with depth |
| **code‑review** | Structured code‑review checklist with actionable feedback | Security, performance, maintainability |
| **code‑refactor** | Minimal‑output refactor – improved structure, naming, error handling | No prose, just the clean code |
| **correctness‑check** | Fact‑check a block of text (with evidence sources) | Works in the same language as the input |
| **deep‑understand** | Funnel‑style requirement discovery for precise, customized answers | Iterative, priority‑driven questioning |
| **fact‑check** | Separate factual claims from evidence (with source links) | Automatic citation, then offers deep analysis |
| **feynman‑tech** | “Explain it like I’m five” learning loop | Simplifies, uncovers knowledge gaps, iterates |
| **find‑me‑experts** | Simulate diverse stakeholder viewpoints on a topic | 3‑5 distinct expert voices |
| **GEO** | Generative Engine Optimization – make content LLM‑friendly | SEO for large language models |
| **grill‑me** | Brutal critical‑thinking coach – no praise, only harsh feedback | Sucker‑punches assumptions, runs failure‑mode checks |
| **improve‑tech‑writing** | Polish academic/technical prose – clear road‑maps, data, no fluff | “Drinkable” writing for reviewers |
| **intention‑detection** | Spot who’s behind a post, why it exists, and how it’s crafted | Motivations, emotional triggers, click‑bait |
| **market‑analysis‑deep‑research** | Four‑step MECE framework for any asset – value sources → constraints → signals | No price targets, just the research skeleton |
| **nice‑advice** | Friendly, “dad‑free” mentoring style – concrete, actionable, warm | 3‑5 bullet points, relatable analogies |
| **product‑risk** | Bomb‑circle theory – assess quality risk by local economic & regulatory ties | Systematic, three‑part judgement |
| **prompt‑enhance** | Elevate scattered instructions into higher‑order principles | Decompose → cluster → elevate → synthesize |
| **readability‑enhance** | Straight‑forward prose polishing – keep meaning, fix grammar | Quick fix, no extra info |
| **real‑business‑simulator** | Night‑mare‑level startup simulation – cash burn, conversion, churn | Interactive decision‑point drama |
| **system‑analysis‑framework** | “Triple Lens” – trace cause, design coordination, gauge impact | Works with incomplete information |
| **talk‑normal** | Direct, positive phrasing – no negations or contrastive language | Clean, assertive communication |
| **transcript‑article** | Turn raw transcripts into a lively business‑tech story | Hooks, gold‑quotes, methodology, takeaways |
| **transcript‑correct** | Clean, read‑ready podcast dialogue – speaker labels, minimal edits | Preserve audio verbatim |
| **transcript‑structured** | Full blog‑post version of a transcript – metadata, overview, sections | Structures for SEO & reading |
| **寓言叙事** | Allegorical narrative – turn a grad‑level concept into a story | Explain then unpack |
| **业务prompt撰写专家** | Business‑prompt writer – abstract generic prompts from specific scenarios | Niche for B2B prompt engineering |
| **深度需求挖掘** | Same as *deep‑understand* (Chinese) | Funnel‑style discovery in Chinese |
| **费曼学习** | Same as *feynman‑tech* (Chinese) | Simplified explanation loop |
| **让大模型更容易推荐你的内容或产品** | Same as *GEO* (Chinese) | LLM‑friendly formatting |
| **字幕->文章** | Same as *transcript‑article* (Chinese) | Business‑tech narrative |
| **字幕->纠错转写** | Same as *transcript‑correct* (Chinese) | Podcast dialogue |
| **字幕->结构化文章** | Same as *transcript‑structured* (Chinese) | Full article with sections |
| **生成式引擎优化 (GEO)** | Same as *GEO* (duplicate entry) | – |
| **构建任何应用的技巧** | Same as *build‑any‑app‑tech‑co* (Chinese) | Technical co‑founder workflow |
| **炸弹项圈理论评估产品风险** | Same as *product‑risk* (Chinese) | Risk assessment via local ties |

> **Tip:** Many prompts are bilingual (Chinese/English). Pick the one that matches the language you’re working in.

---

## 🚀 Installation

1. **Clone the config**

   ```bash
   git clone https://github.com/zsxh/emacs.d.git ~/dotfiles
   ```

2. **Link it into your Emacs directory**

   ```bash
   ln -sfn ~/dotfiles site-lisp/emacs.d
   # or add to your ~/.emacs or init.el:
   add-to-load-path (expand-file-name "site-lisp/emacs.d" user-emacs-directory)
   ```

3. **Ensure Emacs ≥ 28** (the `gptel` package is included in built‑in Elpa, but you may need to enable it)

4. **Install `gptel`** (if not already present)

   ```elisp
   (package-refresh-contents)
   (package-install 'gptel)
   ```

5. **Set your LLM API keys** (e.g., OpenAI, Anthropic, Azure, etc.)  

   Add something like this to your `init.el`:

   ```elisp
   (setq gptel-api-key "sk-...")
         gptel-model "gpt-4o"
         gptel-prompt-functions '(gptel-prompt-from-file) ; loads prompts from this repo
   ```

6. **Load the prompt library** – you can point `gptel-prompt-functions` at the directory containing the `.md` snippets, or add a custom command like:

   ```elisp
   (defun my/load-gptel-prompts ()
     (interactive)
     (let ((dir (expand-file-name "site-lisp/gptel-prompts" user-emacs-directory)))
       (dolist (f (directory-files dir t "\\.md$"))
         (let ((name (file-name-base f)))
           (puthash name (with-temp-buffer
                               (insert-file-contents f)
                               (buffer-string))
                    gptel-prompt-cache))))
     (message "Loaded %d prompts" (hash-table-size gptel-prompt-cache)))

   (my/load-gptel-prompts)
   ```

   Then use `M-x gptel` and select a prompt from the cached list.

---

## 💡 Basic Usage

| Command | Description |
|---------|-------------|
| `M-x gptel` | Start the interactive GPTel buffer. |
| `M-x my-gptel-prompt` | Prompt the cached templates (e.g., `code‑review`, `ask‑ai`). |
| `C-c C-p` (`gptel-prompt-send`) | Send the selected prompt to the model. |
| `C-c C-r` (`gptel-reload`) | Re‑load the prompt files from disk (useful when you edit them). |

**Example workflow (code review):**

1. `M-x gptel` → `M-x my-gptel-prompt` → choose **code‑review**  
2. Paste your source code into the prompt buffer.  
3. `C-c C-p` → view the AI’s review (structured with issues, suggestions, and examples).

**Example workflow (business analysis):**

1. `M-x gptel` → `M-x my-gptel-prompt` → choose **market‑analysis‑deep‑research**  
2. Fill in the target asset (stock, product, startup).  
3. Send → receive a **MECE skeleton** with value‑source breakdown, constraints, and leading indicators.

---

## 📂 Available Prompt Categories

- **General AI Interaction** – `ask‑ai`, `grill‑me`, `nice‑advice`
- **Code & Technical Workflows** – `code‑explain`, `code‑review`, `code‑refactor`, `feynman‑tech`
- **Fact‑checking & Validation** – `correctness‑check`, `fact‑check`
- **Requirement Discovery** – `deep‑understand`, `intention‑detection`
- **Market & Product Research** – `market‑analysis‑deep‑research`, `product‑risk`, `real‑business‑simulator`
- **Content & Writing** – `improve‑tech‑writing`, `GEO`, `transcript‑article`, `transcript‑correct`, `transcript‑structured`
- **Systemic Thinking** – `system‑analysis‑framework`, `prompt‑enhance`, `readability‑enhance`
- **Language‑specific (Chinese)** – same themes as above, prefixed with Chinese titles

Feel free to fork, extend, or submit new prompt templates. All contributions are welcome!

---

## 🔧 Configuration Tips

- **Key bindings** – Add custom bindings in `init.el`:

  ```elisp
  (global-set-key (kbd "C-c p") 'my-gptel-prompt)
  (global-set-key (kbd "C-c r") 'gptel-prompt-send)
  ```

- **Prompt caching** – The cache lives in `gptel-prompt-cache`. If you edit a `.md` file, run `M-x gptel-reload` to refresh.

- **Multi‑model support** – Switch models per prompt by setting `gptel-model` locally, e.g.:

  ```elisp
  (defvar my-gptel-model "claude-3-opus")
  ;; inside a specific prompt function
  (gptel-request :system (format "Model: %s" my-gptel-model)
                 :query prompt-text
                 :done #'my-handle-response)
  ```

---

## 📝 Contributing

1. Fork the repo.  
2. Add new `.md` prompts under `site-lisp/gptel-prompts/`.  
3. Keep the file name short, descriptive, and language‑neutral (or mirror existing naming).  
4. Open a pull request – a brief description of the prompt’s purpose is enough.

---

## 📄 License

```text
MIT License – feel free to use, modify, and share.
```
