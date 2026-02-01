# 测试编写中应避免的错误

## Emacs Lisp 语法相关

### 1. 向量元素分隔符错误
**错误**：
```elisp
(vector "String" "int")  ; 错误：在字面量语法中使用逗号
["String", "int"]        ; 错误！
```

**正确**：
```elisp
(vector "String" "int")  ; 使用 vector 函数
```

或者使用字面量语法时不加逗号：
```elisp
["String" "int"]  ; 字面量语法，元素间用空格分隔
```

**说明**：Emacs Lisp 的向量字面量语法中，元素之间使用空格分隔，而不是逗号。使用逗号会导致 `Invalid read syntax` 错误。

### 2. 空向量表示
**推荐**：
```elisp
(vector)  ; 明确的函数调用，更安全
```

**避免**：
```elisp
[]  ; 某些情况下可能导致解析问题
```

### 3. lambda 可选参数声明
**错误**：
```elisp
(lambda (items prompt display-fn multiple-p transform-fn &rest _args)
  ...)
```

**正确**：
```elisp
(lambda (items prompt display-fn &optional multiple-p transform-fn)
  ...)
```

**说明**：使用 `&optional` 来声明可选参数，而不是将它们放在必需参数列表中。

## 测试数据结构构建

### 4. 复杂嵌套结构的括号平衡
**问题**：构建深层嵌套的 plist 结构时容易遗漏闭括号。

**解决方法**：
- 使用工具检查括号平衡：
  ```bash
  emacs --batch --eval "(check-parens \"file.el\")"
  ```
- 或者用 Python 脚本：
  ```python
  with open('file.el', 'r') as f:
      content = f.read()
      open_count = content.count('(')
      close_count = content.count(')')
      print(f"Diff: {open_count - close_count}")
  ```

### 5. 测试数据复用
**避免**：每个测试用例重复构建相同的数据结构。

**推荐**：提取公共部分：
```elisp
(let* ((field-plist (list :name "service" :type "SomeService"))
       (delegate-methods (vector ...))
       (test-delegate-fields (vector (list :field field-plist :delegateMethods delegate-methods))))
  ...)
```

## Mock 函数设计

### 6. Mock 返回值类型不匹配
**问题**：mock 函数返回的数据类型与实际代码期望的不一致。

**注意**：
- 检查原函数期望返回的是 list 还是 vector
- 例如 `:java/checkDelegateMethodsStatus` 返回的 `:delegateFields` 应该是 vector

### 7. 选择器 prompt 匹配条件
**问题**：多个 `eglot-jdtls--select` 调用时，prompt 匹配条件不够精确。

**推荐**：使用更具体的匹配字符串：
```elisp
((string-match "Select target to generate delegates" prompt) ...)
((string-match "Select methods to generate delegates" prompt) ...)
```

## 调试技巧

### 8. 使用条件错误处理捕获预期错误
```elisp
(condition-case err
    (eglot-jdtls--generate-delegate-methods-prompt-support mock-server test-arguments)
  ((error wrong-type-argument)
   (message "Expected error when no delegate fields: %s" err)))
```

### 9. 收集调用记录进行验证
```elisp
(let ((calls nil))
  (cl-letf (((symbol-function 'some-func)
             (lambda (&rest args)
               (push args calls))))
    (run-test)
    (should (= (length calls) expected-count))))
```

## MVP 原则

### 10. 测试用例数量
- 遵循 MVP 设计原则
- 不追求大而全
- 专注于核心功能的覆盖
- 每次只实现、测试一个测试用例

## cl-letf* 和 Mock 相关

### 11. cl-letf* lambda 无法访问外层 let 变量
**错误**：
```elisp
(let ((test-edit '(:changes [])))
  (cl-letf* (((symbol-function 'some-func)
              (lambda ()
                (should (equal edit test-edit)))))  ; void-variable error!
    ...))
```

**原因**：`cl-letf*` 使用临时函数绑定，lambda 表达式定义时无法捕获外层 `let` 的变量。

**正确**：将测试数据直接内联到 lambda 中：
```elisp
(let ((mock-server (make-hash-table :test 'equal)))
  (cl-letf* (((symbol-function 'some-func)
              (lambda (edit)
                (should (equal edit '(:changes []))))))
    ...))
```

或者使用标志变量验证行为：
```elisp
(let ((mock-server (make-hash-table :test 'equal))
      (func-called nil))
  (cl-letf* (((symbol-function 'some-func)
              (lambda (&rest _args)
                (setq func-called t))))
    (run-test)
    (should func-called)))
```

### 12. 依赖动态变量的值在 batch 模式下可能不可靠
**错误**：
```elisp
(cl-letf* (((symbol-function 'eglot--apply-workspace-edit)
            (lambda (edit command)
              (should (equal command 'this-command)))))  ; batch 模式下为 nil!
  ...)
```

**原因**：`this-command` 等动态变量在 batch 模式下通常为 `nil`。

**正确**：验证函数被调用，而非依赖变量值：
```elisp
(let ((edit-applied nil))
  (cl-letf* (((symbol-function 'eglot--apply-workspace-edit)
              (lambda (edit command)
                (setq edit-applied t)
                (should (equal edit expected-edit)))))
    (run-test)
    (should edit-applied)))
```

### 13. 验证"不应调用"的函数需要使用标志变量
**错误**：
```elisp
(cl-letf* (((symbol-function 'eglot-execute)
            (lambda (&rest _args)
              (should-error "eglot-execute should not be called"))))  ; 永远不会触发!
  ...)
```

**原因**：如果函数未被调用，lambda 中的 `should-error` 永远不会执行。

**正确**：使用标志变量验证未被调用：
```elisp
(let ((eglot-execute-called nil))
  (cl-letf* (((symbol-function 'eglot-execute)
              (lambda (&rest _args)
                (setq eglot-execute-called t))))
    (run-test)
    (should (not eglot-execute-called))))
```

### 14. 调用顺序验证时注意 push 的顺序
**问题**：使用 `push` 记录调用时，后调用的函数会先出现在列表中。

**正确理解**：
```elisp
(let ((call-order nil))
  (cl-letf* (((symbol-function 'func-a)
              (lambda () (push 'a call-order)))
             ((symbol-function 'func-b)
              (lambda () (push 'b call-order))))
    (run-test)
    ;; 如果调用顺序是 a -> b
    ;; call-order 结果是 '(b a) (后进先出)
    (should (equal call-order '(b a)))))
```

## 文件编辑相关

### 15. 使用 Edit 工具时注意保持括号平衡
**问题**：使用 Edit 工具编辑包含嵌套括号的代码时，容易因为复制粘贴或上下文选择不当导致括号数量不正确。

**错误示例**：
```elisp
;; 编辑时将：
(should (not result))))
;; 改成：
(should (not result)))))  ; 多了一个括号！
```

**解决方法**：
- 编辑后立即运行测试验证语法正确性
- 使用 `emacs --batch --eval "(check-parens \"file.el\")"` 检查括号平衡
- 注意 old_string 和 new_string 的括号数量必须一致
- 如果不确定，可以扩大编辑范围，包含完整的函数定义

**最佳实践**：
```bash
# 编辑文件后立即验证
make test
```

### 16. 文件解析错误但括号数量正确
**现象**：出现 "End of file during parsing" 错误，但 Python 检查显示括号数量匹配。

**可能原因**：
- 编辑过程中的临时状态
- 隐藏字符或编码问题
- 缓冲区未刷新

**解决方法**：
- 重新写入整个文件
- 使用 `cat -A` 检查隐藏字符
- 确保文件以换行符结尾

## Mock message 函数

### 17. Mock message 时应捕获格式化后的结果
**错误**：
```elisp
(let ((message-calls nil))
  (cl-letf* (((symbol-function 'message)
              (lambda (format-string &rest args)
                (push (cons format-string args) message-calls))))
    (run-test)
    (should (string= (car (car message-calls)) "%s"))  ; 可能失败!
    (should (string= (cadr (car message-calls)) "expected text"))))
```

**问题**：这种写法将格式字符串和参数分开存储，但在验证时容易出现混淆，而且无法直接验证最终显示给用户的消息内容。

**正确**：使用 `format` 函数获取格式化后的结果：
```elisp
(let ((message-called nil)
      (message-content nil))
  (cl-letf* (((symbol-function 'message)
              (lambda (format-string &rest args)
                (setq message-called t)
                (setq message-content (apply #'format format-string args)))))
    (run-test)
    (should message-called)
    (should (string= message-content "expected text"))))
```

**说明**：使用 `(apply #'format format-string args)` 可以得到最终格式化后的字符串，这样验证更直接、更可靠。

## 显示格式验证

### 18. 验证格式化输出时需要运行测试获取实际输出
**错误**：
```elisp
;; 假设源代码中的格式字符串是：
;; (format "%s %s %s" "[Method Parameter]" "Service" "service")
;; (format "%s %s %s" "[Field]           " "Helper" "helper")  ; 11个空格
(should (member "[Method Parameter] Service service" display-calls))
(should (member "[Field]           Helper helper" display-calls))  ; 可能失败!
```

**问题**：手动计算格式化输出中的空格数量容易出错，特别是当需要对齐不同标签时。

**正确**：先运行测试查看实际输出，然后根据实际输出编写断言：
```elisp
;; 运行测试后发现实际输出是 "[Field]            " (12个空格)
(should (member "[Method Parameter] Service service" display-calls))
(should (member "[Field]            Helper helper" display-calls))  ; 12个空格
```

**说明**：对于涉及格式化对齐的显示函数（如使用不同长度的标签并对齐），应该：
1. 先运行测试并观察失败信息中的实际输出
2. 根据实际输出修正测试用例中的期望值
3. 如果需要，可以在注释中说明空格数量的计算逻辑（如：20字符宽度对齐）

## 序列操作相关

### 19. seq-filter 返回的是 list 而非 vector

**错误**：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (_server method &rest args)
              [(:name "A") (:name "B")]))  ; 返回 vector
           ((symbol-function 'eglot-jdtls--select)
            (lambda (symbols prompt display-fn &rest _args)
              (aref symbols 0)))))  ; 错误！seq-filter 后是 list
```

**问题**：
- `jsonrpc-request` 返回 vector：`[:a :b :c]`
- `seq-filter` 对 vector 操作后返回 list：`(:a :b :c)`
- 使用 `aref` 访问 list 会报错：`wrong-type-argument arrayp`

**正确**：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (_server method &rest args)
              [(:name "A") (:name "B")]))
           ((symbol-function 'eglot-jdtls--select)
            (lambda (symbols prompt display-fn &rest _args)
              (car symbols)))))  ; 使用 car 访问 list
```

**说明**：在 Emacs Lisp 中，`seq-filter`、`seq-map` 等序列操作函数总是返回 list，即使输入是 vector。如果源代码使用了 `seq-filter`，mock 函数中应该使用 `car`/`cdr`/`dolist` 等操作 list 的函数，而不是 `aref`/`vconcat` 等 vector 操作函数。

**调试技巧**：如果遇到 `wrong-type-argument arrayp` 错误，检查：
1. 源代码中是否使用了 `seq-filter`、`seq-map` 等函数
2. Mock 函数中是否正确处理了 list 类型的参数
3. 使用 `(type-of symbols)` 检查实际的数据类型

## Mock 函数完全替换导致内部调用不可见

### 20. Mock 函数会完全替换原函数，内部调用不会被捕获

**错误**：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (server method &rest args)
              (push (list server method) calls)
              ...))
           ((symbol-function 'eglot-jdtls--select-target-class)
            (lambda (server prompt project-name excludes)
              selected-target)))
  (eglot-jdtls--move-static-member mock-server test-arguments)
  ;; 期望看到 2 次调用：:java/searchSymbols 和 :java/move
  (should (= (length calls) 2))  ; 失败！只有 1 次调用
```

**问题**：
- `eglot-jdtls--move-static-member` 调用 `eglot-jdtls--select-target-class`
- 原始的 `eglot-jdtls--select-target-class` 内部会调用 `jsonrpc-request` 发送 `:java/searchSymbols`
- 但因为我们 mock 了 `eglot-jdtls--select-target-class`，整个函数体被替换，内部的 `jsonrpc-request` 调用不会发生
- 所以只能看到直接调用的 `:java/move`

**正确**：只验证被测函数直接调用的方法：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (server method &rest args)
              (push (list server method) calls)
              ...))
           ((symbol-function 'eglot-jdtls--select-target-class)
            (lambda (server prompt project-name excludes)
              selected-target)))
  (eglot-jdtls--move-static-member mock-server test-arguments)
  ;; 只验证 :java/move 被调用
  (should (member :java/move (mapcar #'cadr calls)))
```

**说明**：
- Mock 会完全替换原函数，原函数内部的任何调用都不会经过 mock
- 如果需要验证内部调用的函数，应该为该函数单独编写测试（如 `eglot-jdtls--select-target-class` 已经有自己的测试）
- 在测试上层函数时，关注的是它直接调用的接口，而非间接调用的细节

## plist 与条件判断相关

### 21. plist 中空列表与 nil 的区别

**错误**：
```elisp
;; 源代码中的条件判断：
(when (or (plist-get edit :changes)
          (plist-get edit :documentChanges))
  (eglot-jdtls--refactor-edit server result)
  ...)

;; 测试中使用空列表期望条件为假：
(let ((test-edit '(:changes [] :documentChanges [])))
  ...)
;; 失败！空列表 [] 不是 nil，条件判断为真
```

**问题**：
- 在 Emacs Lisp 中，空列表 `[]` 与 `nil` 是不同的值
- `nil` 是 falsy 值，但空列表 `[]` 是 truthy 值
- `(plist-get '(:changes []) :changes)` 返回 `[]`，不是 `nil`
- `(or [] nil)` 返回 `[]`，在条件判断中被视为真

**正确**：
```elisp
;; 方式1：将值设置为 nil
(let ((test-edit '(:changes nil)))
  ...)

;; 方式2：不包含该键
(let ((test-edit '()))  ; 空 plist
  ...)
```

**说明**：
- 测试"空值"场景时，需要明确区分"键不存在"、"键值为 nil"和"键值为空列表"
- 源代码使用 `(plist-get edit :changes)` 检查键值是否存在且非 nil
- 如果要测试条件为假的情况，应该设置键值为 `nil` 或不包含该键

## cl-letf* 与 Mock 函数相关

### 22. Lambda 返回 list 时多余的闭合括号

**错误**：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (server method &rest args)
              (list :members
                    []
                    :subTypeName "MyInterface"
                    :destinationResponse
                    (list :destinations
                          [(:displayName "com.example" :path "/src/com/example")])))
              ))  ; 错误！这里的 )) 是多余的
           ...))
```

**正确**：
```elisp
(cl-letf* (((symbol-function 'jsonrpc-request)
            (lambda (server method &rest args)
              (list :members
                    []
                    :subTypeName "MyInterface"
                    :destinationResponse
                    (list :destinations
                          [(:displayName "com.example" :path "/src/com/example")])))
              ))  ; 正确，只闭合 lambda
           ...))
```

**说明**：
- Lambda 函数结束时只需要一个 `)` 来闭合
- 如果不小心写成 `))`，第一个 `)` 闭合 lambda，第二个 `)` 会闭合 `cl-letf*` 的参数列表，导致语法错误
- 错误提示通常为：`Invalid read syntax: ")", <line> <column>`

### 23. ert-deftest 中 let + cl-letf* 嵌套的闭合括号层级

**问题**：在测试用例中使用嵌套的 `let` 和 `cl-letf*` 时，闭合括号层级容易混淆。

**结构分析**：
```elisp
(ert-deftest test-name ()
  "Test docstring."
  (let (bindings...)           ; 1. let 开始
    (cl-letf* (mock-bindings...)  ; 2. cl-letf* 开始
      (body-expressions...)       ; 3. cl-letf* body
    )                           ; 4. cl-letf* 结束 (1个括号)
  )                             ; 5. let 结束 (1个括号)
)                               ; 6. ert-deftest 结束 (1个括号)
```

**常见错误**：
```elisp
;; 错误：缺少 cl-letf* body 的闭合括号
(ert-deftest test-name ()
  (let (bindings...)
    (cl-letf* (mock-bindings...)
      (body-expressions...)
    )  ; 只闭合了 cl-letf*
  )    ; 闭合了 let
      ; 缺少 ert-deftest 的闭合括号！
```

**正确写法**：
```elisp
(ert-deftest test-name ()
  (let (bindings...)
    (cl-letf* (mock-bindings...)
      (should ...)
      (should ...))  ; cl-letf* body 的最后表达式不需要额外括号
    )                ; 闭合 cl-letf*
  )                  ; 闭合 let
)                    ; 闭合 ert-deftest
```

**或者使用显式括号**：
```elisp
(ert-deftest test-name ()
  (let (bindings...)
    (cl-letf* (mock-bindings...)
      (progn
        (should ...)
        (should ...)))  ; progn 结束，cl-letf* body 结束
    )                   ; cl-letf* 结束
  ))                    ; let 和 ert-deftest 结束
```

**调试技巧**：
- 使用 Python 脚本检查括号数量：
  ```bash
  python3 << 'EOF'
  with open('file.el', 'r') as f:
      content = f.read()
      open_count = content.count('(')
      close_count = content.count(')')
      print(f"Diff: {open_count - close_count}")
  EOF
  ```
- 对于嵌套结构，逐层检查括号配对
- 在编辑器中使用 `check-parens` 或类似功能

**最佳实践**：
- 保持函数简短，避免过深的嵌套
- 使用 `progn` 显式标记 body 的开始和结束
- 每个测试用例只测试一个功能点，减少嵌套层级

## 测试环境与 Buffer 相关

### 24. 在测试中使用 current-buffer 的问题

**错误**：
```elisp
(let ((test-buffer (current-buffer)))  ; 在外部获取 buffer
  (with-temp-buffer
    ...))
```

**问题**：
- 在 batch 模式下，`(current-buffer)` 返回的 buffer 可能不是预期的测试 buffer
- `with-temp-buffer` 会创建新的临时 buffer，外部获取的 `test-buffer` 可能指向已 killed 的 buffer

**正确**：
```elisp
(with-temp-buffer
  (let ((test-buffer (current-buffer)))  ; 在内部获取 buffer
    ...))
```

**说明**：必须在 `with-temp-buffer` 内部获取 buffer 引用，确保它指向实际的测试 buffer。

### 25. 全局变量在测试间的状态污染

**错误**：
```elisp
(ert-deftest-test-a ()
  (eglot-signature--request :invoked)
  (should eglot-signature--active-buffer))  ; 设置了全局变量

(ert-deftest-test-b ()
  (eglot-signature--request :invoked)
  (should (not eglot-signature--active-buffer)))  ; 失败！上一个测试的值还在
```

**问题**：全局变量在测试之间不会自动重置，导致测试间的状态污染。

**正确**：
```elisp
(ert-deftest-test-b ()
  (let ((active-buffer-before eglot-signature--active-buffer))
    (eglot-signature--request :invoked)
    (should (eq eglot-signature--active-buffer active-buffer-before))))  ; 验证值未改变
```

**说明**：对于可能被其他测试修改的全局变量，应该保存测试前的值，并验证它没有被意外改变，而不是假设它是 nil。

### 26. Mock 内置函数 point 的局限性

**错误**：
```elisp
(cl-letf* (((symbol-function 'point)
            (lambda () 42)))
  (eglot-signature--request :invoked)
  (should (= eglot-signature--active-point 42)))  ; 失败！point 没有被 mock
```

**问题**：`point` 是内置函数（subr），可能在字节编译后被内联，导致 `cl-letf*` 无法有效 mock。

**正确**：
```elisp
(let ((point-before-call (point)))
  (eglot-signature--request :invoked)
  (should (eq eglot-signature--active-point point-before-call)))
```

**说明**：不要尝试 mock `point`、`marker-buffer` 等内置函数。改用记录调用前值的方式验证。

## 条件判断与 Mock 相关

### 27. 复杂条件判断需要确保所有条件满足

**错误**：
```elisp
(ert-deftest test-retrigger ()
  (setq eglot-signature--active-signature mock-sig)
  (eglot-signature--request :trigger-character)
  ;; 验证 retrigger 行为 - 但实际上 retrigger-p 为 nil！
  (should (plist-get context :isRetrigger)))
```

**问题**：`retrigger-p` 的条件是 `(and (not (eq trigger-kind :invoked)) (eglot-signature--sig-active-p) active-sig)`。只设置了 `active-sig`，但 `eglot-signature--sig-active-p` 返回 nil。

**正确**：
```elisp
(cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
            (lambda () t)))
  (setq eglot-signature--active-signature mock-sig)
  (eglot-signature--request :trigger-character)
  (should (plist-get context :isRetrigger)))
```

**说明**：对于复杂的条件判断，需要确保所有条件都满足。仔细阅读源代码中的条件逻辑，mock 所有必要的函数。

## 资源清理相关

### 28. 测试中创建的资源需要及时清理

**错误**：
```elisp
(ert-deftest test-timer-cancel ()
  (let ((mock-timer (timer-create)))
    (setq eglot-signature--debounce-timer mock-timer)
    (cl-letf* (((symbol-function 'cancel-timer)
                (lambda (timer) ...)))
      (eglot-signature--quit)
      (should ...)))
  ;; timer 没有被清理！
```

**问题**：
- `timer-create` 创建真实的 timer 对象
- 即使 mock 了 `cancel-timer`，真实 timer 仍然存在
- 测试结束时没有清理，可能导致资源泄漏

**正确**：
```elisp
(ert-deftest test-timer-cancel ()
  (let ((mock-timer (timer-create)))
    (unwind-protect
        (progn
          (setq eglot-signature--debounce-timer mock-timer)
          (cl-letf* (((symbol-function 'cancel-timer)
                      (lambda (timer) ...)))
            (eglot-signature--quit)
            (should ...)))
      ;; Cleanup: 确保测试结束时清理资源
      (ignore-errors (cancel-timer mock-timer)))))
```

**说明**：
- 使用 `unwind-protect` 确保无论测试成功或失败都会执行清理代码
- `ignore-errors` 防止清理操作本身抛出错误
- 适用于任何创建系统资源的场景（timer、buffer、frame、process 等）

## let 变量绑定顺序相关

### 29. let 绑定列表中后面的变量无法引用前面的变量

**错误**：
```elisp
(let ((active-called nil)
      (sig-help (if (vectorp sig-list)
                    (list :signatures sig-list)
                  mock-sig-help))
      (eglot-signature--active-signature sig-help))  ; void-variable sig-help!
  ...)
```

**问题**：
- 虽然 Emacs Lisp 的 `let` 支持串行求值（`let*`），但在同一个绑定列表中，后面变量的初始化表达式求值时，前面的变量还没有被绑定
- `eglot-signature--active-signature` 试图使用 `sig-help`，但此时 `sig-help` 还没有被赋值

**正确**：使用 `setq` 在 let 体内进行赋值：
```elisp
(let ((active-called nil)
      (sig-help (if (vectorp sig-list)
                    (list :signatures sig-list)
                  mock-sig-help)))
  (setq eglot-signature--active-signature sig-help)
  ...)
```

**或者使用 let* 替代 let**：
```elisp
(let* ((active-called nil)
       (sig-help (if (vectorp sig-list)
                     (list :signatures sig-list)
                   mock-sig-help))
       (eglot-signature--active-signature sig-help))
  ...)
```

**说明**：
- `let` 中所有绑定的初始化表达式是在任何绑定完成之前求值的
- `let*` 中每个绑定的初始化表达式可以看到前面的绑定
- 如果需要引用前面的变量，要么使用 `let*`，要么将相互依赖的赋值放在 let 体内部使用 `setq`

## 测试数据结构设计相关

### 30. 过度使用条件判断构造测试数据会增加复杂度

**问题**：
```elisp
;; 试图用条件判断构造不同的测试场景
(let ((sig-help (if (vectorp sig-list)
                    (list :signatures sig-list :activeSignature 0)
                  mock-sig-help))
      (eglot-signature--active-signature sig-help))
  ...)
```

这种写法：
- 增加了代码复杂度和认知负担
- 容易在条件分支中引入错误
- 难以直观地看出每个测试用例的输入数据

**推荐**：直接在测试用例中包含完整的测试数据：
```elisp
(let ((test-cases '(;; (name sig-active-p valid-win-buf-p sig-help expect-active-called)
                    ("sig-active-p returns nil"
                     nil t (:signatures [(:label "foo") (:label "bar")]) nil)
                    ("signatures is nil"
                     t t (:signatures nil) nil)
                    ("only one signature"
                     t t (:signatures [(:label "foo")]) nil))))
  (dolist (test-case test-cases)
    ...))
```

**优点**：
- 每个测试用例的数据一目了然
- 不需要复杂的条件判断
- 更容易维护和扩展
- 符合"表格驱动测试"的最佳实践

