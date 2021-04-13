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
        let plusFn = liftIntBinary { .Prim(.Int($0 + $1)) }
        let builtin = [
            plusBuiltinType: plusFn,
        ]

        let pgm = gen.App(fn: gen.Constant(plusBuiltinType),
                          args: [
                            gen.Int(1),
                            gen.Int(2),
                          ])


        let expected = gen.Int(3)

        let result = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    /// Define a function, then just return it. Unfortunately, there's not much you can do with it after the evaluator exits.
    func testLambda() throws {
        // This is `const` (\ x y -> x), for what it's worth:
        let pgm = gen.Lambda(arity: 2) { (_, args) in
            return args[0]()
        }

        let expected = gen.Fn(arity: 2)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    /// Define and apply a function in a single expression: `let f(x, y) = x in f(42, 137)`
    func testFunctionCall() throws {
        let pgm = gen.Let(expr:
                            gen.Lambda(arity: 2) { (_, args) in
                                return args[0]()
                            }) { f in
            gen.App(fn: f(),
                    args: [
                        gen.Int(42),
                        gen.Int(137),
                    ])
        }

        let expected = gen.Int(42)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    ///
    func testRecursiveLambda() throws {
        let minusBuiltinType = NodeType("test-builtin", "(-)")
        let minusFn = liftIntBinary { .Prim(.Int($0 - $1)) }
        let timesBuiltinType = NodeType("test-builtin", "(*)")
        let timesFn = liftIntBinary { .Prim(.Int($0 * $1)) }
        let leBuiltinType = NodeType("test-builtin", "(<=)")
        let leFn = liftIntBinary { .Prim(.Bool($0 <= $1)) }
        let builtin = [
            minusBuiltinType: minusFn,
            timesBuiltinType: timesFn,
            leBuiltinType: leFn,
        ]

        // fact(n) = case n <= 1 of
        //             true -> 1
        //             _ -> n * (n - 1)
        let fact = gen.Lambda(arity: 1) { (rec, args) in
            let n = args[0]
            return gen.Match(expr: gen.App(fn: gen.Constant(leBuiltinType),
                                           args: [
                                            n(),
                                            gen.Int(1),
                                           ]),
                             pattern: gen.Bool(true),
                             body: gen.Int(1),
                             otherwise: gen.App(fn: gen.Constant(timesBuiltinType),
                                                args: [
                                                    n(),
                                                    gen.App(fn: rec(),
                                                            args: [gen.App(fn: gen.Constant(minusBuiltinType),
                                                                           args: [
                                                                            n(),
                                                                            gen.Int(1)
                                                                           ])
                                                            ])
                                                ]))
        }

        let pgm = gen.App(fn: fact, args: [ gen.Int(5) ])

        let expected = gen.Int(120)

        let result = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testLet", testLet),
        ("testApp", testApp),
        ("testLambda", testLambda),
        ("testFunctionCall", testFunctionCall),
        ("testRecursiveLambda", testRecursiveLambda),
    ]


// MARK: - Common utility functions

    /// Lift a binary operator to a `Fn` value which will fail if its arguments aren't two ints.
    func liftIntBinary(f: @escaping (Int, Int) -> Node.Value) -> Eval.Value<Node.Value> {
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

            return .Val(f(x, y))
        }
    }

}