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

## 项目结构

```
gua.el/           # 项目文件夹
  ├── gua.el      # 主程序文件
  └── gua.json    # 卦象数据文件
```

## 安装方法

1. 克隆或下载本项目到本地
   ```bash
   git clone https://github.com/your-username/gua.el.git
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

;; 启动时自动运行今日运势占卜
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert "\n\n;; 今日运势\n")
              (insert (gua-divination "今天运势如何？")))))
```

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
  // ... 其他卦象
}
```

## 技术细节

- 使用 Emacs Lisp 实现
- 采用 JSON 格式存储卦象数据
- 使用随机数生成模拟抛钱币过程
- 支持自定义配置和扩展
- 支持 Emacs 启动时自动占卜功能

## 贡献

欢迎提交 Issue 和 Pull Request 来帮助改进这个工具。

## 许可证

本项目采用 MIT 许可证。

