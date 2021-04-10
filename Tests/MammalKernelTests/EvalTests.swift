import XCTest
import MammalKernel

final class EvalTests: XCTestCase {

// MARK: - Basic features

    func testLiteral() throws {
        let env = Eval.Environment<Int>.Empty

        let prg = Eval.Expr<Int>.Literal(0)

        switch try Eval.eval(prg, env: env) {
        case .Val(let result):
            XCTAssertEqual(result, 0)

        default:
            XCTFail("expected Val")
        }
    }

    func testVar() throws {
        let x = Eval.Name(id: 0)

        let env = Eval.Environment<Int>.bindOne(x, to: .Val(0))

        let prg = Eval.Expr<Int>.Var(x)

        switch try Eval.eval(prg, env: env) {
        case .Val(let result):
            XCTAssertEqual(result, 0)

        default:
            XCTFail("expected Val")
        }
    }

    func testLet() throws {
        let x = Eval.Name(id: 0)

        let env = Eval.Environment<Int>.Empty

        let prg = Eval.Expr<Int>.Let(x, expr: .Literal(1), body: .Var(x))

        let result = try Eval.eval(prg, env: env)
        
        XCTAssertTrue(result.isVal(1))
    }

    /// Just define a function (then test by calling it directly).
    func testLambda() throws {
        let x = Eval.Name(id: 0)

        let env = Eval.Environment<Int>.Empty

        // \x -> x
        let prg = Eval.Expr<Int>.Lambda(nil, params: [x], body: .Var(x))

        switch try Eval.eval(prg, env: env) {
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
        let f = Eval.Name(id: 0)

        let identityFn: Eval.Value<Int> = .Fn(arity: 1) { args in args[0] }
        let env = Eval.Environment<Int>.bindOne(f, to: identityFn)

        let prg = Eval.Expr<Int>.App(fn: .Var(f), args: [.Literal(1)])

        let result = try Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a trivial quotation.
    func testQuote() throws {
        let env = Eval.Environment<Int>.Empty

        let prg = Eval.Expr<Int>.Quote(expand: { _ in .Literal(1) })

        let result = try Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a quotation that refers to a value from the current scope: `x + 2`.
    func testQuote1() throws {
        let x = Eval.Name(id: 0)

        let env = Eval.Environment<Int>.bindOne(x, to: .Val(1))

        let prg = Eval.Expr<Int>.Quote(expand: { eval in
            let er = try eval(.Var(x))
            return try er.withVal { val in
                .Literal(val + 2)
            }
        })

        let result = try Eval.eval(prg, env: env)
        XCTAssertTrue(result.isVal(3))
    }

    /// Match any positive int. No bindings.
    func testMatch() throws {
        let x = Eval.Name(id: 0)

        let prg = Eval.Expr<Int>.Match(expr: .Var(x),
                                  bindings: [],
                                  body: .Literal(1),
                                  otherwise: .Literal(0),
                                  match: lift { $0 > 0})

        let positiveResult = try Eval.eval(prg, env: Eval.Environment<Int>.bindOne(x, to: .Val(42)))
        XCTAssertTrue(positiveResult.isVal(1))

        let negativeResult = try Eval.eval(prg, env: Eval.Environment<Int>.bindOne(x, to: .Val(-137)))
        XCTAssertTrue(negativeResult.isVal(0))
    }

// MARK: - Advanced features

    /// Factorial. And yes, it is pretty painful to construct these ASTs by hand.
    func testRecursiveLambda() throws {
        // Define a couple of built-in functions, and add them to a common environment:
        // Note: (<=) can't be defined this way, because there are no boolean values,
        // so we'll just use primitive pattern matching for that.
        let times = Eval.Name(id: 0)
        let timesFn = liftBinary(f: (*))
        let minus = Eval.Name(id: 1)
        let minusFn = liftBinary(f: (-))
        let builtInEnv = Eval.Environment<Int>.Empty
            .with(times, boundTo: timesFn)
            .with(minus, boundTo: minusFn)

        // Now construct `\ n -> if n <= 1 then 1 else n * fact(n-1)`:
        let factRec = Eval.Name(id: 10)
        let n = Eval.Name(id: 11)
        let factFn = try Eval.eval(Eval.Expr<Int>.Lambda(
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
                                        match: lift { $0 <= 1 })),
                               env: builtInEnv)

        let fact = Eval.Name(id: 20)
        let env = builtInEnv.with(fact, boundTo: factFn)

        // Easy case — no recursive call:
        let factOne = try Eval.eval(Eval.Expr<Int>.App(fn: .Var(fact),
                                                       args: [.Literal(0)]),
                                    env: env)
        XCTAssertTrue(factOne.isVal(1))

        // Now for the fun part — let's get recursive!
        let factFive = try Eval.eval(Eval.Expr<Int>.App(fn: .Var(fact),
                                                        args: [.Literal(5)]),
                                     env: env)
        XCTAssertTrue(factFive.isVal(120))
    }

// TODO: tail calls don't use stack

// MARK: - Error cases

// TODO: lots of error cases that should throw a descriptive error and not just die or hang


// MARK: - Common utility functions

    /// Lift a predicate on integers into a `Match` function that doesn't bind any values.
    func lift(predicate: @escaping (Int) -> Bool) -> Eval.MatchAndBind<Int> {
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
    func liftBinary(f: @escaping (Int, Int) -> Int) -> Eval.Value<Int> {
        Eval.Value<Int>.Fn(arity: 2) { args in
            guard args.count == 2 else {
                throw Eval.RuntimeError.ArityError(expected: 2, found: args)
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
