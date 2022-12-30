import XCTest

#if !canImport(ObjectiveC)
public func allTests() -> [XCTestCaseEntry] {
    return [
        testCase(EvalTests.allTests),
        testCase(KernelTests.allTests),
        testCase(ReduceTests.allTests),
        testCase(SimpleTrieTests.allTests),
        testCase(ZipperTests.allTests),
    ]
}
#endif
