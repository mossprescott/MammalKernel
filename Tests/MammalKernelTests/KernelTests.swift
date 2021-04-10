import XCTest
import MammalKernel

final class KernelTests: XCTestCase {

// MARK: - ?

    func testLiteral() throws {
        let pgm = Value.Node(id: NodeId(42), type: Kernel.Nil.type, content: .Empty)

        let result = try Kernel.eval(pgm)

        XCTAssertEqual(pgm.diff(result), [])
    }

    /// `let x = 1 in x`. Hey, these programs are a pain to write out by hand!
    func testLet() throws {
        let pgm = Value.Node(id: NodeId(42),
                             type: Kernel.Let.type,
                             content: .Attrs([
                                Kernel.Let.bind:
                                    .Node(id: NodeId(43),
                                          type: Kernel.Bind.type,
                                          content: .Empty),
                                Kernel.Let.expr:
                                    .Node(id: NodeId(44),
                                          type: Kernel.Int_.type,
                                          content: .Attrs([
                                            Kernel.Int_.value:
                                                .Prim(.Int(137))
                                          ])),
                                Kernel.Let.body:
                                    .Node(id: NodeId(45),
                                          type: Kernel.Var.type,
                                          content: .Ref(NodeId(43)))
                             ]))

        let expected = Value.Node(id: NodeId(0),
                                  type: Kernel.Int_.type,
                                  content: .Attrs([
                                    Kernel.Int_.value:
                                        .Prim(.Int(137))
                                  ]))

        let result = try Kernel.eval(pgm)

        print(result)

        XCTAssertEqual(expected.diff(result), [])
    }

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testLet", testLet),
    ]
}
