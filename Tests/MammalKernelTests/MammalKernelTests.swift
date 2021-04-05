import XCTest
@testable import MammalKernel

final class MammalKernelTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(MammalKernel().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
