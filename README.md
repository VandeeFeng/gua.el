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

## Project Structure

```
gua.el/           # Project folder
  ├── gua.el      # Main program file
  └── gua.json    # Hexagram data file
```

## Installation

1. Clone or download this project
   ```bash
   git clone https://github.com/your-username/gua.el.git
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

;; Run daily fortune divination at startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert "\n\n;; Daily Fortune\n")
              (insert (gua-divination "How's my fortune today?")))))
```

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

