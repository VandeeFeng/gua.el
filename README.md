[中文文档](README_zh.md)

inspired by https://github.com/RealKai42/liu-yao-divining

# Gua - Yi Jing Divination Tool for Emacs

A traditional Chinese Yi Jing divination tool implemented in Emacs. It simulates the traditional six-line divination method by digitally mimicking the coin tossing process, providing users with Yi Jing divination results.

## Features

- 🎲 Simulates traditional six-line divination coin tossing process
- 📚 Complete interpretation of all 64 hexagrams
- 🔄 Automatically generates upper and lower trigrams
- 💡 Detailed hexagram explanations and interpretations
- ⚡ Support for displaying results at current point or in *scratch* buffer
- 🌐 Uses JSON for hexagram data storage, easy to extend and maintain
- 🔮 Supports automatic daily fortune divination at startup
- 🤖 Integrated LLM support for advanced interpretations (Ollama, OpenAI, OpenRouter)

## Project Structure

```
gua.el/           # Project folder
  ├── gua.el      # Main program file
  └── gua.json    # Hexagram data file
```

## Installation

1. Clone or download this project
   ```bash
   git clone https://github.com/VandeeFeng/gua.el.git
   # or download and extract the zip file
   ```

2. Add the project directory to your Emacs `load-path`. Add the following to your Emacs configuration file:
   ```elisp
   ;; Replace "/path/to/gua.el" with the actual project path
   (add-to-list 'load-path "/path/to/gua.el")
   (require 'gua.el)
   ```

## Usage

### Basic Usage

1. Use `M-x gua` to start divination
2. Enter your question
3. The system will automatically generate divination results, including:
   - Details of each line
   - Names of upper and lower trigrams
   - Final hexagram interpretation
   - Related divination text

### Customization

You can customize the tool's behavior through the following variables:

```elisp
;; Set result display location
(setq gua-insert-at-point t)  ; Display results at current cursor position
;; or
(setq gua-insert-at-point nil)  ; Display in *scratch* buffer (default)

;; Data file location (optional)
;; By default, uses gua.json in the project directory
;; To use a custom gua.json file, set:
(setq gua-data-directory "/your/custom/path")

;; LLM Integration Configuration
;; ----------------------------

;; 1. Enable LLM integration
(setq gua-llm-enabled t)  ; Enable LLM integration
;; or
(setq gua-llm-enabled nil)  ; Disable LLM integration (default)

;; 2. Choose LLM service
(setq gua-llm-service 'ollama)    ; Use Ollama (default) - local LLM service
(setq gua-llm-service 'openai)    ; Use OpenAI API - requires API key
(setq gua-llm-service 'openrouter); Use OpenRouter API - requires API key
(setq gua-llm-service 'custom)    ; Use custom LLM service

;; 3. Configure service-specific settings

;; For Ollama:
(setq gua-llm-model "qwen2.5:14b")  ; Or any other Ollama model
;; Endpoint is set to "http://localhost:11434/api/generate" by default
;; No API key needed for Ollama

;; For OpenAI:
(setq gua-llm-model "gpt-4-turbo-preview")  ; Or "gpt-3.5-turbo", etc.
(setq gua-llm-api-key "your-openai-api-key")
;; Endpoint will be automatically set to "https://api.openai.com/v1/chat/completions"
;; Or you can set custom endpoint:
(setq gua-llm-endpoint "https://your-custom-openai-endpoint")

;; For OpenRouter:
(setq gua-llm-model "anthropic/claude-3-opus")  ; Or any other supported model
(setq gua-llm-api-key "your-openrouter-api-key")
;; Endpoint will be automatically set to "https://openrouter.ai/api/v1/chat/completions"
;; Or you can set custom endpoint:
(setq gua-llm-endpoint "https://your-custom-openrouter-endpoint")

;; For Custom Service:
(setq gua-llm-service 'custom)
(setq gua-llm-model "your-model-name")
(setq gua-llm-api-key "your-api-key")
(setq gua-llm-endpoint "https://your-custom-endpoint")  ; Required for custom service

;; 4. Optional: Customize prompts
;; System prompt for LLM context
(setq gua-llm-system-prompt "Your custom system prompt")

;; User prompt template for formatting questions and results
(setq gua-llm-default-user-prompt "Your custom user prompt template")

;; Run daily fortune divination at startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert "\n\n;; Daily Fortune\n")
              ;; Use async divination with callback
              (gua-divination 
               "How's my fortune today?"
               (lambda (result)
                 (with-current-buffer "*scratch*"
                   (goto-char (point-max))
                   (insert result)))))))
```

### LLM Integration Details

The tool supports three LLM services out of the box:

1. **Ollama** (Default)
   - Local LLM service
   - No API key required
   - Supports various models like Qwen, LLaMA, etc.
   - Default endpoint: `http://localhost:11434/api/generate`

2. **OpenAI**
   - Cloud-based service
   - Requires API key from [OpenAI Platform](https://platform.openai.com)
   - Supports models like GPT-4, GPT-3.5
   - Default endpoint: `https://api.openai.com/v1/chat/completions`
   - Supports custom endpoint configuration

3. **OpenRouter**
   - Multi-model API gateway
   - Requires API key from [OpenRouter](https://openrouter.ai)
   - Access to various models from different providers
   - Default endpoint: `https://openrouter.ai/api/v1/chat/completions`
   - Supports custom endpoint configuration

4. **Custom Service**
   - Fully customizable configuration
   - Requires manual setup of endpoint, model, and API key
   - Follows the same JSON request/response format as OpenAI

The service endpoint is automatically configured when you change `gua-llm-service`, but you can always override it by setting `gua-llm-endpoint` manually. Basic setup requires:
1. The service (`gua-llm-service`)
2. The model name (`gua-llm-model`)
3. API key if required (`gua-llm-api-key`)
4. Custom endpoint if needed (`gua-llm-endpoint`)

## Data File

### Location

The `gua.json` file is located in the project directory by default. To use a data file in a different location, set the `gua-data-directory` variable.

### Format

The `gua.json` file should contain data in the following format:

```json
{
  "QianKun": {
    "name": "Tai",
    "des": "Hexagram description",
    "sentence": "Divination text"
  },
}
```

## Technical Details

- Implemented in Emacs Lisp
- Uses JSON format for hexagram data storage
- Employs random number generation for coin toss simulation
- Supports custom configuration and extensions
- Includes automatic startup divination feature

## Contributing

Issues and Pull Requests are welcome to help improve this tool.

## License

This project is licensed under the GNU General Public License v3.0.

## Note

This tool is designed for entertainment purposes only. Please use your own judgment for important decisions.

