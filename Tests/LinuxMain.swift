import XCTest

import MammalKernelTests

var tests = [XCTestCaseEntry]()
tests += EvalTests.allTests()
XCTMain(tests)
