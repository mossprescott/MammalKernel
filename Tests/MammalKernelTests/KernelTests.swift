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

    func testQuoteTrivial() throws {
        let body = gen.Constant(NodeType("test-example", "foo"))

        let pgm = gen.Quote(body: body)

        let expected = body

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    func testQuoteSingleUnquote() throws {
        let pgm = gen.Quote(body:
                                gen.Unquote(expr:
                                                gen.Int(42)))

        let expected = gen.Int(42)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    /// Construct a node by pulling a couple of values which are provided as evaluation-time constants.
    func testQuoteAttrPrimitives() throws {
        let personType = NodeType("test-example", "person")
        let nameAttr = AttrName(personType, "name")
        let ageAttr = AttrName(personType, "age")

        let nameConstantType = NodeType("test-builtin", "name")
        let nameConstantValue: Eval.Value<Node.Value> = .Val(.Prim(.String("Steve")))
        let ageConstantType = NodeType("test-builtin", "age")
        let ageConstantValue: Eval.Value<Node.Value> = .Val(.Prim(.Int(42)))
        let builtin = [
            nameConstantType: nameConstantValue,
            ageConstantType: ageConstantValue,
        ]

        let pgm = gen.Quote(body:
                                Node(NodeId(1000),
                                     personType,
                                     .Attrs([
                                        nameAttr:
                                            .Node(gen.Unquote(expr: gen.Constant(nameConstantType))),
                                        ageAttr:
                                            .Node(gen.Unquote(expr: gen.Constant(ageConstantType))),
                                     ])))

        let expected = Node(NodeId(2000),
                            personType,
                            .Attrs([
                                nameAttr: .Prim(.String("Steve")),
                                ageAttr: .Prim(.Int(42)),
                            ]))

        let result = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    func testUnquoteSplice() throws {
        func intNode(_ x: Int) -> Node {
            return Node(IdGen.Shared.generateId(),
                        NodeType("test", "intValue"),
                        .Attrs([
                            AttrName("test", "value"):
                                .Prim(.Int(x))
                        ]))
        }

        let intsConstantType = NodeType("test-builtin", "ints")
        let intsConstantValue: Eval.Value<Node.Value> =
            .Val(.Node(
                    Node(NodeId(1),
                         NodeType("test", "ints"),
                         .Elems([
                            intNode(1),
                            intNode(2),
                            intNode(3),
                         ]))))
        let builtin = [
            intsConstantType: intsConstantValue,
        ]

        let pgm = gen.Quote(body: Node(NodeId(1000),
                                       NodeType("test", "values"),
                                       .Elems([
                                        intNode(0),
                                        gen.UnquoteSplice(expr: gen.Constant(intsConstantType)),
                                        intNode(4)
                                       ])))

        let expected = Node(NodeId(2000),
                            NodeType("test", "values"),
                            .Elems([
                                intNode(0),
                                intNode(1),
                                intNode(2),
                                intNode(3),
                                intNode(4),
                            ]) as Node.Content)

        let result = try Kernel.eval(pgm, constants: builtin)

        print(result.debugDescription)

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

//    /// Quoted lambda with interpolation happening within the body. This is the typical case for
//    /// macro expansion. For simplicity, the captured value is just a constant.
//    func testQuoteLambda() throws {
//        let capturedType = NodeType("test-builtin", "x")
//        let capturedValue: Eval.Value<Node.Value> = .Val(.Prim(.Int(42)))
//        let builtin = [
//            capturedType: capturedValue,
//        ]
//
//        let pgm = gen.Quote(body: gen.Lambda(paramNames: ["_"]) { _, _ in
//            gen.Unquote(expr: gen.Constant(capturedType))
//        })
//
//        let result = try Kernel.eval(pgm, constants: builtin)
//
//        print(result)
//
//        XCTFail("\(result)")
//    }

    func testQuotedQuote() throws {
        let fType = NodeType("test-builtin", "f")  // Note: never actually evaluated.
        let builtin: [NodeType: Eval.Value<Node.Value>] = [:]

        // kernel/int, but accepting an arbitrary node (i.e. an unquote)
        func genInt(_ val: Node) -> Node {
            Node(IdGen.Shared.generateId(),
                 Kernel.Int_.type,
                 .Attrs([
                    Kernel.Int_.value: .Node(val),
                 ]))
        }

        let pgm = gen.Quote(
            body: gen.Quote(
                body: gen.App(fn: gen.Constant(fType),
                              args: [
                                gen.Int(2),

                                // Still quoted:
                                genInt(gen.Unquote(expr: gen.Int(1))),
                              ])))

        let expected1 = gen.Quote(
            body: gen.App(fn: gen.Constant(fType),
                          args: [
                            gen.Int(2),
                            genInt(gen.Unquote(expr: gen.Int(1))),
                          ]))

        let result1 = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected1, to: result1), [])


        // Now evaluate a second time to eliminate the second-level of quotation:

        let expected2 = gen.App(fn: gen.Constant(fType),
                               args: [
                                   gen.Int(2),
                                   gen.Int(1),
                               ])

        let result2 = try Kernel.eval(result1, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected2, to: result2), [])
    }

    func testQuotedQuoteWithUnquotedUnquote() throws {
        let fType = NodeType("test-builtin", "f")  // Note: never actually evaluated.
        let builtin: [NodeType: Eval.Value<Node.Value>] = [:]

        // kernel/int, but accepting an arbitrary node (i.e. an unquote)
        func genInt(_ val: Node) -> Node {
            Node(IdGen.Shared.generateId(),
                 Kernel.Int_.type,
                 .Attrs([
                    Kernel.Int_.value: .Node(val),
                 ]))
        }

        let pgm = gen.Quote(
            body: gen.Quote(
                body: gen.App(fn: gen.Constant(fType),
                              args: [
                                gen.Int(2),

                                // Still quoted:
                                genInt(gen.Unquote(expr: gen.Int(1))),

                                // Unquoted twice to get back to the outer context:
                                genInt(gen.Unquote(expr: gen.Unquote(expr: gen.Int(0)))),
                              ])))

        let expected1 = gen.Quote(body:
                                    gen.App(fn: gen.Constant(fType),
                                            args: [
                                                gen.Int(2),
                                                genInt(gen.Unquote(expr: gen.Int(1))),
                                                gen.Int(0),
                                            ]))

        let result1 = try Kernel.eval(pgm, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected1, to: result1), [])


        // Now evaluate a second time to eliminate the second-level of quotation:

        let expected2 = gen.App(fn: gen.Constant(fType),
                                args: [
                                    gen.Int(2),
                                    gen.Int(1),
                                    gen.Int(0),
                                ])

        let result2 = try Kernel.eval(result1, constants: builtin)

        XCTAssertEqual(Diff.changes(from: expected2, to: result2), [])
    }

    func testMatchTrivial() throws {
        let pgm = gen.Match(expr: gen.Int(42),
                            pattern: gen.Int(42),
                            body: gen.Bool(true),
                            otherwise: gen.Bool(false))

        let expected = gen.Bool(true)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    func testMatchTrivialNoMatch() throws {
        let pgm = gen.Match(expr: gen.Int(42),
                            pattern: gen.Int(43),
                            body: gen.Bool(true),
                            otherwise: gen.Bool(false))

        let expected = gen.Bool(false)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }

    // TODO: sort out exactly how Bind nodes get (un)quoted within patterns.
    // How do you match a Bind?
    func testMatchBindAttr() throws {
        let (valBind, valRef) = gen.bindGen()

        let fooType = NodeType("test", "foo")
        let barAttr = AttrName(fooType, "bar")

        // case `foo { bar: 42 }` of
        //   foo { bar: x } -> x
        //   _ -> nil
        let pgm = gen.Match(expr: gen.Quote(body:
                                                Node(IdGen.Shared.generateId(),
                                                     fooType,
                                                     .Attrs([
                                                        barAttr:
                                                            .Prim(.Int(42))
                                                     ]))),
                            pattern:
                                Node(IdGen.Shared.generateId(),
                                     fooType,
                                     .Attrs([
                                        barAttr:
                                            .Node(valBind)
                                     ])),
                            body: valRef(),
                            otherwise: gen.Nil())

        let expected = gen.Int(42)

        let result = try Kernel.eval(pgm, constants: [:])

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])
    }


// TODO: error cases

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testLet", testLet),
        ("testApp", testApp),
        ("testLambda", testLambda),
        ("testFunctionCall", testFunctionCall),
        ("testRecursiveLambda", testRecursiveLambda),
        ("testQuoteTrivial", testQuoteTrivial),
        ("testQuoteSingleUnquote", testQuoteSingleUnquote),
        ("testQuoteAttrPrimitives", testQuoteAttrPrimitives),
        ("testMatchTrivial", testMatchTrivial),
        ("testMatchTrivialNoMatch", testMatchTrivialNoMatch),
        ("testMatchBindAttr", testMatchBindAttr),

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
