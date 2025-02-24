inspired by https://github.com/RealKai42/liu-yao-divining

# Gua - Emacs 易经六爻占卜工具

这是一个在 Emacs 中实现的易经六爻占卜工具。它使用传统的六爻占卜方法，通过数字化的方式模拟抛钱币卜卦过程，为用户提供易经六爻占卜的结果。

## 功能特点

- 🎲 模拟传统的六爻占卜抛钱币过程
- 📚 完整的六十四卦象解读
- 🔄 自动生成上卦和下卦
- 💡 详细的卦象解释和卦辞说明
- ⚡ 支持在当前位置或 *scratch* buffer 中显示结果
- 🌐 使用 JSON 数据存储卦象信息，易于扩展和维护
- 🔮 支持启动时自动占卜今日运势
- 🤖 集成 LLM 支持，提供高级解读（支持 Ollama、OpenAI、OpenRouter）

## 项目结构

```
gua.el/           # 项目文件夹
  ├── gua.el      # 主程序文件
  └── gua.json    # 卦象数据文件
```

## 安装方法

1. 克隆或下载本项目到本地
   ```bash
   git clone https://github.com/VandeeFeng/gua.el.git
   # 或下载 zip 文件并解压
   ```

2. 将项目目录添加到 Emacs 的 `load-path` 中。在您的 Emacs 配置文件中添加：
   ```elisp
   ;; 将 "/path/to/gua.el" 替换为实际的项目路径
   (add-to-list 'load-path "/path/to/gua.el")
   (require 'gua.el)
   ```

## 使用方法

### 基本使用

1. 使用 `M-x gua` 启动占卜
2. 输入您的问题
3. 系统会自动生成占卜结果，包括：
   - 每一爻的具体情况
   - 上卦和下卦的名称
   - 最终卦象的解释
   - 相关的卦辞

### 自定义配置

您可以通过以下变量来自定义工具的行为：

```elisp
;; 设置结果显示位置
(setq gua-insert-at-point t)  ; 在当前光标位置显示结果
;; 或
(setq gua-insert-at-point nil)  ; 在 *scratch* buffer 中显示结果（默认）

;; 数据文件位置（可选配置）
;; 默认会使用项目目录下的 gua.json
;; 如果需要使用自定义的 gua.json 文件，可以设置：
(setq gua-data-directory "/your/custom/path")

;; LLM 集成配置
;; ------------

;; 1. 启用 LLM 集成
(setq gua-llm-enabled t)  ; 启用 LLM 集成
;; 或
(setq gua-llm-enabled nil)  ; 禁用 LLM 集成（默认）

;; 2. 选择 LLM 服务
(setq gua-llm-service 'ollama)    ; 使用 Ollama（默认）- 本地 LLM 服务
(setq gua-llm-service 'openai)    ; 使用 OpenAI API - 需要 API 密钥
(setq gua-llm-service 'openrouter); 使用 OpenRouter API - 需要 API 密钥
(setq gua-llm-service 'custom)    ; 使用自定义 LLM 服务

;; 3. 配置服务特定设置

;; Ollama 配置：
(setq gua-llm-model "qwen2.5:14b")  ; 或其他 Ollama 模型
;; 端点默认设置为 "http://localhost:11434/api/generate"
;; Ollama 不需要 API 密钥

;; OpenAI 配置：
(setq gua-llm-model "gpt-4-turbo-preview")  ; 或 "gpt-3.5-turbo" 等
(setq gua-llm-api-key "your-openai-api-key")
;; 端点会自动设置为 "https://api.openai.com/v1/chat/completions"
;; 或者您可以设置自定义端点：
(setq gua-llm-endpoint "https://your-custom-openai-endpoint")

;; OpenRouter 配置：
(setq gua-llm-model "anthropic/claude-3-opus")  ; 或其他支持的模型
(setq gua-llm-api-key "your-openrouter-api-key")
;; 端点会自动设置为 "https://openrouter.ai/api/v1/chat/completions"
;; 或者您可以设置自定义端点：
(setq gua-llm-endpoint "https://your-custom-openrouter-endpoint")

;; 自定义服务配置：
(setq gua-llm-service 'custom)
(setq gua-llm-model "your-model-name")
(setq gua-llm-api-key "your-api-key")
(setq gua-llm-endpoint "https://your-custom-endpoint")  ; 自定义服务必需设置

;; 4. 可选：自定义提示词
;; LLM 系统提示词
(setq gua-llm-system-prompt "你的自定义系统提示词")

;; 用户提示词模板，用于格式化问题和结果
(setq gua-llm-default-user-prompt "你的自定义用户提示词模板")

;; 启动时自动运行今日运势占卜
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert "\n\n;; 今日运势\n")
              ;; 使用异步占卜并在回调中处理结果
              (gua-divination 
               "今天运势如何？"
               (lambda (result)
                 (with-current-buffer "*scratch*"
                   (goto-char (point-max))
                   (insert result)))))))
```

### LLM 集成详情

本工具内置支持三种 LLM 服务：

1. **Ollama**（默认）
   - 本地 LLM 服务
   - 无需 API 密钥
   - 支持多种模型，如 Qwen、LLaMA 等
   - 默认端点：`http://localhost:11434/api/generate`

2. **OpenAI**
   - 云端服务
   - 需要 [OpenAI Platform](https://platform.openai.com) 的 API 密钥
   - 支持 GPT-4、GPT-3.5 等模型
   - 默认端点：`https://api.openai.com/v1/chat/completions`
   - 支持自定义端点配置

3. **OpenRouter**
   - 多模型 API 网关
   - 需要 [OpenRouter](https://openrouter.ai) 的 API 密钥
   - 可访问多个提供商的各种模型
   - 默认端点：`https://openrouter.ai/api/v1/chat/completions`
   - 支持自定义端点配置

4. **自定义服务**
   - 完全可自定义配置
   - 需要手动设置端点、模型和 API 密钥
   - 遵循与 OpenAI 相同的 JSON 请求/响应格式

当您更改 `gua-llm-service` 时，服务端点会自动配置，但您也可以通过手动设置 `gua-llm-endpoint` 来覆盖默认配置。基本设置需要：
1. 服务类型（`gua-llm-service`）
2. 模型名称（`gua-llm-model`）
3. 如果需要的话，设置 API 密钥（`gua-llm-api-key`）
4. 如果需要的话，设置自定义端点（`gua-llm-endpoint`）

## 数据文件说明

### 位置

`gua.json` 文件默认位于项目目录下。如果需要使用其他位置的数据文件，可以通过设置 `gua-data-directory` 变量来指定。

### 格式

`gua.json` 文件应包含以下格式的数据：

```json
{
  "乾坤": {
    "name": "泰",
    "des": "卦象描述",
    "sentence": "卦辞"
  },
}
```

## 技术细节

- 使用 Emacs Lisp 实现
- 采用 JSON 格式存储卦象数据
- 使用随机数生成模拟抛钱币过程
- 支持自定义配置和扩展
- 支持 Emacs 启动时自动占卜功能
- 集成多种 LLM 服务，支持智能解读

## 贡献

欢迎提交 Issue 和 Pull Request 来帮助改进这个工具。

## 许可证

本项目采用 GNU General Public License v3.0 许可证。

## 注意

本工具仅供娱乐目的，请勿过分依赖其占卜结果。重要决定请谨慎判断。