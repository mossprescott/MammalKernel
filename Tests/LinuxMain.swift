import XCTest

import MammalKernelTests

var tests = [XCTestCaseEntry]()
tests += KernelTests.allTests()
XCTMain(tests)
