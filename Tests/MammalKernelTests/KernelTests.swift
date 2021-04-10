import XCTest
import MammalKernel

final class KernelTests: XCTestCase {

// MARK: - ?

    func testLiteral() throws {
        let pgm = Value.Node(id: NodeId(42), type: Kernel.Nil.type, content: .Empty)

        let result = try Kernel.eval(pgm)

        print(result)

        XCTAssertEqual(pgm.diff(result), [])
    }

    static var allTests = [
        ("testLiteral", testLiteral),
    ]
}
