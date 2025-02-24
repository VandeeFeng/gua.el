;; -*- lexical-binding: t; -*-

(require 'json)
(require 'time-date)
(require 'url)
(require 'cl-lib)

(defvar gua-dict
  '(("阳阳阳" . "乾")
    ("阴阴阴" . "坤") 
    ("阴阳阳" . "兑")
    ("阳阴阳" . "震")
    ("阳阳阴" . "巽")
    ("阴阳阴" . "坎")
    ("阳阴阴" . "艮")
    ("阴阴阳" . "离")))

(defvar gua-number-dict
  '((0 . "初爻")
    (1 . "二爻")
    (2 . "三爻")
    (3 . "四爻")
    (4 . "五爻")
    (5 . "六爻")))

(defvar gua-unicode-dict
  '(("乾" . "☰")  ; 天
    ("坤" . "☷")  ; 地
    ("震" . "☳")  ; 雷
    ("坎" . "☵")  ; 水
    ("艮" . "☶")  ; 山
    ("巽" . "☴")  ; 风
    ("离" . "☲")  ; 火
    ("兑" . "☱")) ; 泽
  "Dictionary for mapping trigram names to their Unicode symbols.")

(defvar gua-hexagram-dict
  '(("乾乾" . "䷀")
    ("坤坤" . "䷁")
    ("震坎" . "䷂")
    ("坎艮" . "䷃")
    ("坎乾" . "䷄")
    ("乾坎" . "䷅")
    ("坤坎" . "䷆")
    ("坎坤" . "䷇")
    ("巽乾" . "䷈")
    ("兑乾" . "䷉")
    ("乾坤" . "䷊")
    ("坤乾" . "䷋")
    ("乾离" . "䷌")
    ("离乾" . "䷍")
    ("坤艮" . "䷎")
    ("震坤" . "䷏")
    ("兑震" . "䷐")
    ("艮巽" . "䷑")
    ("坤兑" . "䷒")
    ("巽坤" . "䷓")
    ("离震" . "䷔")
    ("艮离" . "䷕")
    ("艮坤" . "䷖")
    ("坤震" . "䷗")
    ("震乾" . "䷘")
    ("乾艮" . "䷙")
    ("艮震" . "䷚")
    ("兑巽" . "䷛")
    ("坎坎" . "䷜")
    ("离离" . "䷝")
    ("兑艮" . "䷞")
    ("震巽" . "䷟")
    ("艮乾" . "䷠")
    ("震乾" . "䷡")
    ("离坤" . "䷢")
    ("坤离" . "䷣")
    ("巽离" . "䷤")
    ("离兑" . "䷥")
    ("坎艮" . "䷦")
    ("震坎" . "䷧")
    ("兑艮" . "䷨")
    ("巽震" . "䷩")
    ("兑乾" . "䷪")
    ("乾兑" . "䷫")
    ("坤兑" . "䷬")
    ("兑坤" . "䷭")
    ("坎兑" . "䷮")
    ("坎巽" . "䷯")
    ("兑离" . "䷰")
    ("巽离" . "䷱")
    ("震震" . "䷲")
    ("艮艮" . "䷳")
    ("巽艮" . "䷴")
    ("震兑" . "䷵")
    ("离震" . "䷶")
    ("艮离" . "䷷")
    ("巽巽" . "䷸")
    ("兑兑" . "䷹")
    ("坎巽" . "䷺")
    ("兑坎" . "䷻")
    ("巽兑" . "䷼")
    ("艮震" . "䷽")
    ("坎离" . "䷾")
    ("离坎" . "䷿"))
  "Dictionary for mapping hexagram names to their Unicode symbols.")

(defgroup gua nil
  "Yi Jing divination tool for Emacs."
  :group 'applications)

;; Define variables that will be used by functions
(defvar gua-llm-service 'ollama
  "The LLM service to use. Do not set this directly, use `gua-llm-service' custom variable instead.")

(defvar gua-llm-endpoint nil
  "The endpoint URL for LLM API calls. Do not set this directly, use `gua-llm-endpoint' custom variable instead.")

(defvar gua-llm-callback nil
  "Callback function to handle LLM response.")

;; Define all functions first
(defun gua-get-default-endpoint ()
  "Get the default endpoint URL based on the current LLM service."
  (cond
   ((eq gua-llm-service 'ollama)
    "http://localhost:11434/api/generate")
   ((eq gua-llm-service 'openai)
    "https://api.openai.com/v1/chat/completions")
   ((eq gua-llm-service 'openrouter)
    "https://openrouter.ai/api/v1/chat/completions")
   (t nil)))

(defun gua-set-llm-endpoint ()
  "Set the correct endpoint URL based on the selected LLM service."
  (let ((default-endpoint (gua-get-default-endpoint)))
    (when default-endpoint
      (setq gua-llm-endpoint default-endpoint))))

;; Now define customization variables
(defcustom gua-data-directory nil
  "Directory containing gua.json file.
If nil, will use the same directory as where gua.el is installed.
You can set this to a specific directory to override the default behavior."
  :type '(choice (const :tag "Use default location" nil)
                (directory :tag "Custom directory"))
  :group 'gua)

(defcustom gua-insert-at-point nil
  "If non-nil, insert divination result at current point instead of *scratch* buffer.
If nil, insert result in *scratch* buffer as before."
  :type 'boolean
  :group 'gua)

(defcustom gua-llm-enabled nil
  "If non-nil, enable LLM integration for divination interpretation."
  :type 'boolean
  :group 'gua)

(defcustom gua-llm-service 'ollama
  "The LLM service to use.
Currently supported services: ollama, openai, openrouter, custom."
  :type '(choice (const :tag "Ollama" ollama)
                (const :tag "OpenAI" openai)
                (const :tag "OpenRouter" openrouter)
                (const :tag "Custom Service" custom))
  :set (lambda (sym val)
         (set-default sym val)
         (gua-set-llm-endpoint))
  :group 'gua)

(defcustom gua-llm-model "qwen2.5:14b"
  "The model to use for LLM queries.
For ollama, this would be the model name (e.g. 'qwen', 'llama2', etc.).
For custom services, this would be the model identifier required by the service."
  :type 'string
  :group 'gua)

(defcustom gua-llm-api-key nil
  "API key for LLM service.
Not needed for Ollama, but required for most other services."
  :type '(choice (const :tag "Not needed" nil)
                (string :tag "API Key"))
  :group 'gua)

(defcustom gua-llm-system-prompt
  "You are a professional I Ching (易经) divination interpreter. Your task is to interpret the divination result in Chinese.

IMPORTANT RULES:
1. Extract and use the EXACT symbols and names from the input:
   - Hexagram name: extract from the line starting with '卦名为：'
   - Upper trigram: extract from the line starting with '首卦为：'
   - Lower trigram: extract from the line starting with '次卦为：'
   - Full hexagram symbol: extract from the line starting with '六爻结果'
2. NEVER convert Chinese characters to pinyin or ASCII
3. NEVER modify or rewrite any symbols
4. Keep all interpretations in Chinese language

Output Format:
1. 卦象概述
   - 卦名：[从结果中提取的卦名]
   - 上卦：[从结果中提取的上卦名和符号]
   - 下卦：[从结果中提取的下卦名和符号]
   - 卦象：[从结果中提取的完整卦象符号]
   - 含义：[简要说明]

2. 核心信息
   [核心信息概述]

3. 详细解读
   - 卦象构成
     * 上卦：[上卦解释]
     * 下卦：[下卦解释]
     * 关系：[上下卦关系]
   - 主题分析
     [核心主题解释]
   - 问题解答
     [针对问题的具体解释]

4. 实践指导
   [具体建议]

5. 总结
   [总结和最终建议]

Note: Always maintain the exact formatting of Chinese characters and symbols as they appear in the input. Do not attempt to convert or modify any symbols."
  "System prompt for LLM when interpreting divination results."
  :type 'string
  :group 'gua)

(defcustom gua-llm-default-user-prompt
  "Interpret the following I Ching divination result. Keep all Chinese characters and symbols exactly as they appear:

Question:
%s

Divination Result:
%s

Important: Preserve all Chinese characters, trigram symbols (e.g. ☰,☷,etc), and hexagram symbols (e.g. ䷊) exactly as shown above."
  "Default user prompt template for LLM.
%s will be replaced with the question and divination result."
  :type 'string
  :group 'gua)

(defun gua-set-random-seed ()
  "Set random seed based on current time"
  (random t))

(defun gua-find-info (gua-key gua-data)
  "Find gua info from gua-data by key"
  (let ((result nil))
    (maphash (lambda (k v)
               (when (string= k gua-key)
                 (setq result v)))
             gua-data)
    result))

(defun gua-get-data-directory ()
  "Get the directory containing gua.json.
If gua-data-directory is set, use that.
Otherwise, use the project root directory (the directory containing this file)."
  (or gua-data-directory
      (let ((lib-path (locate-library "gua")))
        (if lib-path
            (file-name-directory lib-path)
          (error "Cannot locate gua.el library")))))

(defun gua-load-json ()
  "Load gua.json file"
  (let* ((json-file (expand-file-name "gua.json" (gua-get-data-directory)))
         (_ (message "尝试加载文件: %s" json-file)))
    (if (file-exists-p json-file)
        (let* ((json-string (with-temp-buffer
                            (insert-file-contents json-file)
                            (buffer-string)))
               (json-object-type 'hash-table)  ; 使用哈希表
               (json-array-type 'list)
               (data (json-read-from-string json-string)))
          (message "成功加载 JSON 数据，包含 %d 个卦象" (hash-table-count data))
          data)
      (error "找不到 gua.json 文件: %s\n请确保文件存在并设置正确的 gua-data-directory 变量" json-file))))

(defun gua-get-3-coin ()
  "Get 3 random coins (0 or 1)"
  (mapcar (lambda (_) (random 2)) '(1 2 3)))

(defun gua-get-yin-yang (coin-result)
  "Convert coin result to yin/yang"
  (if (> (apply '+ coin-result) 1.5) "阳" "阴"))

(defun gua-format-coin-result (coin-result i)
  "Format the coin toss result"
  (format "%s 为 %s 为 %s"
          (cdr (assoc i gua-number-dict))
          (mapconcat (lambda (x) (if (> x 0.5) "背" "字")) coin-result "")
          (gua-get-yin-yang coin-result)))

(defun gua-get-unicode-symbol (gua-name)
  "Get Unicode symbol for a trigram name."
  (or (cdr (assoc gua-name gua-unicode-dict))
      gua-name))

(defun gua-get-hexagram-symbol (gua-key)
  "Get Unicode symbol for a hexagram."
  (or (cdr (assoc gua-key gua-hexagram-dict))
      gua-key))

(defun gua-format-llm-input (question divination-result)
  "Format the input for LLM query.
QUESTION is the original divination question.
DIVINATION-RESULT is the raw divination result."
  (format gua-llm-default-user-prompt
          question
          divination-result))

(defun gua-divination (question callback)
  "Do a gua divination for the given question asynchronously.
QUESTION is the divination question.
CALLBACK is a function that will be called with the final divination result."
  (gua-set-random-seed)
  (let* ((gua-data (gua-load-json))
         (_ (message "数据类型: %s" (type-of gua-data)))
         (first-results '())
         (second-results '())
         (output '())
         first-gua
         second-gua)
    
    ;; Add question to output
    (push (format "问题：%s\n" question) output)
    
    ;; First three coins
    (dotimes (i 3)
      (let* ((coins (gua-get-3-coin))
             (yin-yang (gua-get-yin-yang coins)))
        (push yin-yang first-results)
        (push (gua-format-coin-result coins i) output)))
    
    ;; 处理首卦
    (let* ((first-gua-key (mapconcat 'identity (reverse first-results) ""))
           (_ (message "首卦键值: %s" first-gua-key))
           (first-gua-lookup (assoc first-gua-key gua-dict))
           (_ (message "首卦查找结果: %s" first-gua-lookup)))
      (setq first-gua (if first-gua-lookup
                         (cdr first-gua-lookup)
                       (progn
                         (message "未找到首卦: %s" first-gua-key)
                         "未知")))
      (push (format "首卦为：%s %s" first-gua (gua-get-unicode-symbol first-gua)) output))
    
    ;; Second three coins  
    (dotimes (i 3)
      (let* ((coins (gua-get-3-coin))
             (yin-yang (gua-get-yin-yang coins)))
        (push yin-yang second-results)
        (push (gua-format-coin-result coins (+ i 3)) output)))
    
    ;; 处理次卦和结果
    (let* ((second-gua-key (mapconcat 'identity (reverse second-results) ""))
           (_ (message "次卦键值: %s" second-gua-key))
           (second-gua-lookup (assoc second-gua-key gua-dict))
           (_ (message "次卦查找结果: %s" second-gua-lookup)))
      (setq second-gua (if second-gua-lookup
                          (cdr second-gua-lookup)
                        (progn
                          (message "未找到次卦: %s" second-gua-key)
                          "未知")))
      
      (let* ((gua-key (concat first-gua second-gua))
             (_ (message "完整卦象键值: %s" gua-key))
             (_ (message "正在查找卦象: %s" gua-key))
             (gua-info (gethash gua-key gua-data))
             (_ (message "查找结果: %s" gua-info))
             (gua-name (if gua-info
                          (gethash "name" gua-info)
                        (format "未知卦象：%s" gua-key)))
             (_ (message "卦名: %s" gua-name))
             (gua-des (if gua-info
                         (gethash "des" gua-info)
                       "无法解析此卦象"))
             (_ (message "卦象描述: %s" gua-des))
             (gua-sentence (if gua-info
                              (gethash "sentence" gua-info)
                            "请检查卦象数据"))
             (_ (message "卦辞: %s" gua-sentence)))
        
        (push (format "次卦为：%s %s" second-gua (gua-get-unicode-symbol second-gua)) output)
        (push (format "\n六爻结果: %s (%s%s) %s\n卦名为：%s\n%s\n卦辞为：%s"
                     gua-key
                     (gua-get-unicode-symbol first-gua)
                     (gua-get-unicode-symbol second-gua)
                     (gua-get-hexagram-symbol gua-key)
                     gua-name
                     gua-des
                     gua-sentence)
              output)))
    
    (let ((divination-text (mapconcat 'identity (reverse output) "\n")))
      (if gua-llm-enabled
          (progn
            (message "LLM 占卜中...")
            (cl-flet ((handle-llm-response 
                      (llm-response)
                      (funcall callback
                              (concat divination-text
                                     "\n\nLLM 解读：\n"
                                     llm-response
                                     "\n\n切勿迷信，仅供娱乐"))))
              (gua-llm-query 
               gua-llm-system-prompt
               (gua-format-llm-input question divination-text)
               #'handle-llm-response)))
        (funcall callback divination-text)))))

(defun gua ()
  "Interactive gua divination.
If `gua-insert-at-point' is non-nil, insert result at current point.
Otherwise, insert in *scratch* buffer."
  (interactive)
  (let ((question (read-string "请输入你的问题: ")))
    (message "正在进行占卜，请稍候...")
    (gua-divination 
     question
     (lambda (result)
       (let ((result-text (concat "\n\n" result)))
         (if gua-insert-at-point
             (insert result-text)
           (with-current-buffer (get-buffer-create "*scratch*")
             (goto-char (point-max))
             (insert result-text)))
         (message "占卜完成！"))))))

(defun gua-llm-handle-response (status callback)
  "Handle the LLM response in the callback buffer.
STATUS is the callback status from url-retrieve.
CALLBACK is the function to call with the response text."
  (if (or (not status) ;; No error
          (eq (car status) :redirect)) ;; Handle redirects
      (let (response-json response-text)
        (set-buffer-multibyte t)
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point-min) (1+ (point)))
        (condition-case err
            (progn
              (setq response-json (json-read))
              (setq response-text
                    (cond
                     ((eq gua-llm-service 'ollama)
                      (or (cdr (assoc 'response response-json))
                          (error "No response field in Ollama output")))
                     ((eq gua-llm-service 'openai)
                      (or (cdr (assoc 'content
                                     (cdr (assoc 'message
                                               (aref (cdr (assoc 'choices response-json)) 0)))))
                          (error "No content in OpenAI response")))
                     ((eq gua-llm-service 'openrouter)
                      (or (cdr (assoc 'content
                                     (cdr (assoc 'message
                                               (aref (cdr (assoc 'choices response-json)) 0)))))
                          (error "No content in OpenRouter response")))
                     (t
                      (or (cdr (assoc 'content (aref (cdr (assoc 'choices response-json)) 0)))
                      (error "No content in response")))))
              (message "LLM 解读完成，正在生成结果...")
              (funcall callback (decode-coding-string response-text 'utf-8)))
          (error
           (message "LLM 解读出错：%s" (error-message-string err))
           (funcall callback (format "Error parsing JSON response: %s" (error-message-string err))))))
    (message "LLM 请求出错：%s" status)
    (funcall callback (format "Error in HTTP request: %s" status)))
  (kill-buffer))

(defun gua-llm-query (system-prompt user-prompt callback)
  "Query LLM with given prompts asynchronously.
SYSTEM-PROMPT is the system context.
USER-PROMPT is the user's query.
CALLBACK is a function that will be called with the response text."
  (when (and (not (eq gua-llm-service 'ollama))
             (null gua-llm-api-key))
    (error "LLM API key not set for %s service" gua-llm-service))
  
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (append
           '(("Content-Type" . "application/json"))
           (cond
            ((eq gua-llm-service 'ollama) nil)
            ((eq gua-llm-service 'openai)
             `(("Authorization" . ,(concat "Bearer " gua-llm-api-key))))
            ((eq gua-llm-service 'openrouter)
             `(("Authorization" . ,(concat "Bearer " gua-llm-api-key))))
            (t `(("Authorization" . ,(concat "Bearer " gua-llm-api-key)))))))
         (request-data
          (cond
           ((eq gua-llm-service 'ollama)
            `((model . ,gua-llm-model)
              (prompt . ,(format "%s\n\n%s" system-prompt user-prompt))
              (stream . :json-false)
              (options . ((temperature . 0.7)))))
           ((eq gua-llm-service 'openai)
            `((model . ,gua-llm-model)
              (messages . [((role . "system")
                          (content . ,system-prompt))
                         ((role . "user")
                          (content . ,user-prompt))])
              (temperature . 0.7)
              (stream . :json-false)))
           ((eq gua-llm-service 'openrouter)
            `((model . ,gua-llm-model)
              (messages . [((role . "system")
                          (content . ,system-prompt))
                         ((role . "user")
                          (content . ,user-prompt))])
              (temperature . 0.7)))
           (t
            `((model . ,gua-llm-model)
              (messages . [((role . "system")
                          (content . ,system-prompt))
                         ((role . "user")
                          (content . ,user-prompt))])))))
         (url-request-data
          (encode-coding-string
           (json-encode request-data)
           'utf-8)))
    (url-retrieve gua-llm-endpoint
                 #'gua-llm-handle-response
                 (list callback)
                 t t)))

(provide 'gua.el) 