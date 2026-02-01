# 编写测试用例规范

> 对于简单的方法(比如不需要mock的，或者mock验证形式比较固定的)，应该尽可能使用单个测试方法而非多个独立的测试方法，在方法中先定义测试集合，然后遍历测试它

按以下步骤进行:
1. 分析需要测试的方法的功能，然后执行第 2 步;
2. 根据需要测试的方法的功能，设计若干独立的测试用例，测试用例记录在 @tests/test-todos.md 当中，以checkbox的形式记录，然后执行第 3 步;
3. 检查 @tests/test-todos.md 中是否存在未完成的测试用例，如果存在未完成测试用例，仅选择其中一个执行第 4 步，如果全部完成，结束任务。
4. 在 @tests/eglot-signature-test-temperature.el 中实现选中的未完成的测试用例，然后执行第 5 步;
5. 运行测试，检查实现是否存在问题，如果存在问题，修复问题直至运行测试通过。测试通过后将测试用例迁移到 @tests/eglot-signature-test.el 中，然后继续执行第3步;

# 注意事项

- 对于 eglot/jsonrpc/completing-read等需要网络/本地io/用户交互的方法，都需要进行Mock
- 测试用例数量满足mvp设计原则，不追求大而全
- 每次开始实现新的测试示例前，都需要读取 @docs/test-rule.md 文档明确规范
- @tests/eglot-signature-test-temperature.el 中每次只实现、测试一个测试用例，测试通过后立刻将测试用例移动到 @tests/eglot-signature-test.el
- 可以用 make test 运行所有测试用例
- 对于简单的方法(比如不需要mock的，或者mock验证形式比较固定的)，应该尽可能使用单个测试方法而非多个独立的测试方法，在方法中先定义测试集合，然后遍历测试它

# 其他问题

@docs/mistakes-should-avoid.md
