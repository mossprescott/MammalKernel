import XCTest
import MammalKernel

final class EvalTests: XCTestCase {

    typealias Expr = Eval.Expr<UInt, Int>
    typealias Value = Eval.Value<UInt, Int>
    typealias Env = [UInt: Value]

// MARK: - Basic features

    func testLiteral() throws {
        let env: Env = [:]

        let prg = Expr.Literal(0)

        try Eval.eval(prg, env: env)
            .withVal { result in
                XCTAssertEqual(result, 0)
            }
    }

    func testVar() throws {
        let x: UInt = 0
        
        let env: Env = [x: .Val(0)]
        
        let prg = Expr.Var(x)
        
        try Eval.eval(prg, env: env)
            .withVal { result in
                XCTAssertEqual(result, 0)
            }
    }

    func testLet() throws {
        let x: UInt = 0

        let env: Env = [:]

        let prg = Expr.Let(x, expr: .Literal(1), body: .Var(x))

        let result = Eval.eval(prg, env: env)
        
        XCTAssertTrue(result.isVal(1))
    }

    /// Just define a function (then test by calling it directly).
    func testLambda() throws {
        let x: UInt = 0

        let env: Env = [:]

        // \x -> x
        let prg = Expr.Lambda(nil, params: [x], body: .Var(x))

        switch Eval.eval(prg, env: env) {
        case .Fn(let arity, let f):
            XCTAssertEqual(arity, 1)

            let result = try f([.Val(1)])
            XCTAssertTrue(result.isVal(1))

        default:
            XCTFail("expected Val")
        }
    }

    /// Apply a function that's available in the environment.
    func testApp() throws {
        let f: UInt = 0

        let identityFn: Value = .Fn(arity: 1) { args in args[0] }
        let env: Env = [f: identityFn]

        let prg = Expr.App(fn: .Var(f), args: [.Literal(1)])

        let result = Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a trivial quotation.
    func testQuote() throws {
        let env: Env = [:]

        let prg = Expr.Quote(expand: { _ in .Val(1) })

        let result = Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a quotation that refers to a value from the current scope: `x + 2`.
    func testQuote1() throws {
        let x: UInt = 0

        let env: Env = [x: .Val(1)]

        let prg = Expr.Quote(expand: { eval in
            let er = try eval(.Var(x))
            return try er.withVal { val in
                .Val(val + 2)
            }
        })

        let result = Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(3))
    }

    /// Match any positive int. No bindings.
    func testMatch() throws {
        let x: UInt = 0

        let prg = Expr.Match(expr: .Var(x),
                                  bindings: [],
                                  body: .Literal(1),
                                  otherwise: .Literal(0),
                                  match: lift { $0 > 0})

        let positiveResult = Eval.eval(prg, env: [x: .Val(42)])
        XCTAssertTrue(positiveResult.isVal(1))

        let negativeResult = Eval.eval(prg, env: [x: .Val(-137)])
        XCTAssertTrue(negativeResult.isVal(0))
    }

// MARK: - Advanced features

    /// Factorial. And yes, it is pretty painful to construct these ASTs by hand.
    func testRecursiveLambda() throws {
        // Define a couple of built-in functions, and add them to a common environment:
        // Note: (<=) can't be defined this way, because there are no boolean values,
        // so we'll just use primitive pattern matching for that.
        let times: UInt = 0
        let timesFn = liftBinary(f: (*))
        let minus: UInt = 1
        let minusFn = liftBinary(f: (-))
        let builtInEnv: Env = [
            times: timesFn,
            minus: minusFn
            ]

        // Now construct `\ n -> if n <= 1 then 1 else n * fact(n-1)`:
        let factRec: UInt = 10
        let n: UInt = 11
        let factFn = Expr.Lambda(
            factRec,
            params: [n],
            body:
                    .Match(  // if n <= 1 then 1 else ...
                        expr: .Var(n),
                        bindings: [],
                        body: .Literal(1),
                        otherwise:
                                .App(fn: .Var(times),  // n * fact(n - 1)
                                     args: [
                                        .Var(n),
                                        .App(fn: .Var(factRec),  // fact(n - 1)
                                             args: [
                                                .App(fn: .Var(minus),  // n - 1
                                                     args: [
                                                        .Var(n),
                                                        .Literal(1)
                                                     ])
                                             ])
                                     ]),
                        match: lift { $0 <= 1 }))

//        let fact: UInt = 20

        // Easy case — no recursive call:
        let factOne = Eval.eval(Expr.App(fn: factFn,
                                        args: [.Literal(0)]),
                                    env: builtInEnv)
        XCTAssertTrue(factOne.isVal(1))

        // Now for the fun part — let's get recursive!
        let factFive = Eval.eval(Expr.App(fn: factFn,
                                          args: [.Literal(5)]),
                                     env: builtInEnv)
        XCTAssertTrue(factFive.isVal(120))
    }

    /// A function can call itself a million times, if the calls are in tail position.
    func testTailRecursion() throws {
        let plus: UInt = 0
        let plusFn = liftBinary(f: (+))
        let builtInEnv: Env = [plus: plusFn]

        // Tail-recursive version:
        //   f 0 acc = acc
        //   f n acc = f (n - 1) (acc + n)
        let fnRec: UInt = 1
        let n: UInt = 2
        let acc: UInt = 3
        let fn = Expr.Lambda(fnRec,
                             params: [n, acc],
                             body: Expr.Match(expr: .Var(n),
                                              bindings: [],
                                              body: .Var(acc),
                                              otherwise: .App(fn: Eval.Expr.Var(fnRec),
                                                              args: [
                                                                .App(fn: .Var(plus),
                                                                     args: [.Var(n), .Literal(-1)]),
                                                                .App(fn: .Var(plus),
                                                                     args: [.Var(acc), .Var(n)]),
                                                              ]),
                                              match: lift { $0 <= 0 }))



        let result1 = Eval.eval(Expr.App(fn: fn, args: [.Literal(6), .Literal(0)]),
                                   env: builtInEnv)
        try result1.withVal { XCTAssertEqual($0, 21) }

        let n2 = 1000*1000
        let result2 = Eval.eval(Expr.App(fn: fn, args: [.Literal(n2), .Literal(0)]),
                                   env: builtInEnv,
                                budget: n2)
        try result2.withVal { XCTAssertEqual($0, n2*(n2+1)/2) }
    }

    func testMutualRecursion() {
        // TODO: a pair of functions that call each other a million times
        XCTFail("TODO")
    }


// MARK: - Error cases

// In each case, the error is captured as an Expr.Val.Error

    func testStackOverflow() throws {
        let plus: UInt = 0
        let plusFn = liftBinary(f: (+))
        let builtInEnv: Env = [plus: plusFn]

        // This function calls itself, but not in tail position, so it builds up partial results
        // on the stack:
        //   f 0 = 0
        //   f n = n + f (n - 1)
        let fnRec: UInt = 1
        let n: UInt = 2
        let fn = Expr.Lambda(fnRec,
                             params: [n],
                             body: Expr.Match(expr: .Var(n),
                                              bindings: [],
                                              body: .Literal(0),
                                              otherwise: .App(fn: .Var(plus),
                                                              args: [
                                                                .Var(n),
                                                                .App(fn: Eval.Expr.Var(fnRec),
                                                                     args: [
                                                                        .App(fn: .Var(plus),
                                                                             args: [.Var(n), .Literal(-1)]),
                                                                     ])
                                                              ]),
                                              match: lift { $0 <= 0 }))



        let result1 = Eval.eval(Expr.App(fn: fn, args: [.Literal(6)]),
                                   env: builtInEnv)
        try result1.withVal { XCTAssertEqual($0, 21) }

        let n2 = 1000*1000
        let result2 = Eval.eval(Expr.App(fn: fn, args: [.Literal(n2)]),
                                   env: builtInEnv)
        try result2.withVal { XCTAssertEqual($0, n2*(n2+1)/2) }

    }

    /// This function makes a tail-recursive call, so uses no stack, but also never makes any progress, so it should get killed.
    func testInfiniteLoop() throws {
//        let expect = expectation(description: "killed")

        let fnRec: UInt = 1
        let fn = Expr.Lambda(fnRec,
                                       params: [],
                                       body: Eval.Expr.App(fn: Eval.Expr.Var(fnRec),
                                                           args: []))
        
        switch Eval.eval(Expr.App(fn: fn, args: []),
                         env: [:]) {
        case .Error(.TimeOut):
            print("Timed out as expected")
            //expect.fulfill()
        default:
            XCTFail("Should have produced an error")
        }
//        result.withError { XCTAssertEqual($0, .TimeOut) }

//        waitForExpectations(timeout: 1)
    }

// TODO: lots of error cases that should throw a descriptive error and not just die or hang


// MARK: - Common utility functions

    /// Lift a predicate on integers into a `Match` function that doesn't bind any values.
    func lift(predicate: @escaping (Int) -> Bool) -> Eval.MatchAndBind<UInt, Int> {
        return { valOrFn in
            try valOrFn.withVal { val in
                if predicate(val) {
                    return .Matched([])
                }
                else {
                    return .NoMatch
                }
            }
        }
    }

    /// Lift a binary operator to a `Fn` value which will fail if its arguments aren't two ints.
    func liftBinary(f: @escaping (Int, Int) -> Int) -> Value {
        Value.Fn(arity: 2) { args in
            guard args.count == 2 else {
                return .Error(Eval.RuntimeError.ArityError(expected: 2, found: args))
            }
            return try args[0].withVal { x in
                try args[1].withVal { y in
                    .Val(f(x, y))
                }
            }
        }
    }

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testVar", testVar),
        ("testLet", testLet),
        ("testLambda", testLambda),
        ("testApp", testApp),
        ("testQuote", testQuote),
        ("testQuote1", testQuote1),
        ("testMatch", testMatch),
        ("testRecursiveLambda", testRecursiveLambda),
    ]
}
