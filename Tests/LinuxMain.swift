import XCTest

import MammalKernelTests

var tests = [XCTestCaseEntry]()
tests += EvalTests.allTests()
tests += KernelTests.allTests()
tests += ReduceTests.allTests()
tests += SimpleTrieTests.allTests()
tests += ZipperTests.allTests()
XCTMain(tests)
