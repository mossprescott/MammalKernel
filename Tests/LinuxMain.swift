import XCTest

import MammalKernelTests

var tests = [XCTestCaseEntry]()
tests += EvalTests.allTests()
tests += KernelTests.allTests()
tests += ReduceTests.allTests()
XCTMain(tests)
