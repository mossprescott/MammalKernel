import XCTest
import MammalKernel

final class KernelTests: XCTestCase {

// MARK: - ?

    let gen = KernelGen()

    func testLiteral() throws {
        let pgm = gen.Nil()

        let result = try Kernel.eval(pgm, constants: [:])

        let expected = pgm

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    /// Trivial: `let x = 137 in x`.
    func testLet() throws {
        let pgm = gen.Let(expr: gen.Int(137)) { ref in
            ref()
        }

        let expected = gen.Int(137)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    /// `plus(1, 2)`,  using a (fake) built-in addition function.
    func testApp() throws {
        let plusBuiltinType = NodeType("test-builtin", "(+)")
        let plusFn = liftIntBinary { $0 + $1 }
        let builtin = [
            plusBuiltinType: plusFn,
        ]

        let pgm = gen.App(fn: Node(NodeId(1001), plusBuiltinType, .Empty),
                          args: [
                            gen.Int(1),
                            gen.Int(2),
                          ])


        let expected = gen.Int(3)

        let result = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testLet", testLet),
        ("testApp", testApp),
    ]


// MARK: - Common utility functions

    /// Lift a binary operator to a `Fn` value which will fail if its arguments aren't two ints.
    func liftIntBinary(f: @escaping (Int, Int) -> Int) -> Eval.Value<Node.Value> {
        Eval.Value<Node.Value>.Fn(arity: 2) { args in
            guard args.count == 2 else {
                throw Eval.RuntimeError.ArityError(expected: 2, found: args)
            }

            var x: Int
            switch args[0] {
            case .Val(.Prim(.Int(let v))):
                x = v
            default:
                return .Error(Eval.RuntimeError.TypeError(expected: "Int", found: args[0]))
            }

            var y: Int
            switch args[1] {
            case .Val(.Prim(.Int(let v))):
                y = v
            default:
                return .Error(Eval.RuntimeError.TypeError(expected: "Int", found: args[1]))
            }

            return .Val(.Prim(.Int(f(x, y))))
        }
    }

}
