import XCTest

import MammalKernelTests

var tests = [XCTestCaseEntry]()
tests += EvalTests.allTests()
tests += KernelTests.allTests()
XCTMain(tests)
