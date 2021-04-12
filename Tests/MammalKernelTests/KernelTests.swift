import XCTest
import MammalKernel

final class KernelTests: XCTestCase {

// MARK: - ?

    let gen = KernelGen()

    func testLiteral() throws {
        let pgm = gen.Nil()

        let result = try Kernel.eval(pgm)

        XCTAssertEqual(pgm.diff(result), [])
    }

    /// Trivial: `let x = 137 in x`.
    func testLet() throws {
        let pgm = gen.Let(expr: gen.Int(137)) { ref in
            ref()
        }

        let expected = gen.Int(137)

        let result = try Kernel.eval(pgm)

        print(result)

        XCTAssertEqual(expected.diff(result), [])
    }

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testLet", testLet),
    ]
}
