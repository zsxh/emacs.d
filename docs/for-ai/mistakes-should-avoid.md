# 测试编写中应避免的错误

### 向量元素分隔符错误
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

### 空向量表示
**推荐**：
```elisp
(vector)  ; 明确的函数调用，更安全
```

**避免**：
```elisp
[]  ; 某些情况下可能导致解析问题
```

### lambda 可选参数声明
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

### 复杂嵌套结构的括号平衡
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

### 测试数据复用
**避免**：每个测试用例重复构建相同的数据结构。

**推荐**：提取公共部分：
```elisp
(let* ((field-plist (list :name "service" :type "SomeService"))
       (delegate-methods (vector ...))
       (test-delegate-fields (vector (list :field field-plist :delegateMethods delegate-methods))))
  ...)
```

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

### 依赖动态变量的值在 batch 模式下可能不可靠
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

### 验证"不应调用"的函数需要使用标志变量
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

### Mock message 时应捕获格式化后的结果
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

### plist 中空列表与 nil 的区别

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

### 在测试中使用 current-buffer 的问题

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

### 全局变量在测试间的状态污染

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

### Mock 内置函数 point 的局限性

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


### 测试中创建的资源需要及时清理

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

### 过度使用条件判断构造测试数据会增加复杂度

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

