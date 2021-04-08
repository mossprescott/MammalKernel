import XCTest
import MammalKernel

final class EvalTests: XCTestCase {
    func testLiteral() throws {
        let env = Environment<Int>.Empty

        let prg = Expr<Int>.Literal(0)

        switch try eval0(prg, env: env) {
        case .Val(let result):
            XCTAssertEqual(result, 0)

        default:
            XCTFail("expected Val")
        }
    }

    func testVar() throws {
        let x = Name(id: 0)

        let env = Environment<Int>.bindOne(x, to: .Val(0))

        let prg = Expr<Int>.Var(x)

        switch try eval0(prg, env: env) {
        case .Val(let result):
            XCTAssertEqual(result, 0)

        default:
            XCTFail("expected Val")
        }
    }

    func testLet() throws {
        let x = Name(id: 0)

        let env = Environment<Int>.Empty

        let prg = Expr<Int>.Let(x, expr: .Literal(1), body: .Var(x))

        let result = try eval0(prg, env: env)
        
        XCTAssertTrue(result.isVal(1))
    }

    /// Just define a function (then test by calling it directly).
    func testFn() throws {
        let x = Name(id: 0)

        let env = Environment<Int>.Empty

        // \x -> x
        let prg = Expr<Int>.Lambda(nil, params: [x], body: .Var(x))

        switch try eval0(prg, env: env) {
        case .Fn(let arity, let f):
            XCTAssertEqual(arity, 1)

            let result = try f([.Val(1)])
            XCTAssertTrue(result.isVal(1))

        default:
            XCTFail("expected Val")
        }
    }

    /// Define a function externally, then apply it.
    func testApp() throws {
        let f = Name(id: 0)

        let identityFn: Value<Int> = .Fn(arity: 1) { args in args[0] }
        let env = Environment<Int>.bindOne(f, to: identityFn)

        let prg = Expr<Int>.App(fn: .Var(f), args: [.Literal(1)])

        let result = try eval0(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a trivial quotation.
    func testQuote() throws {
        let env = Environment<Int>.Empty

        let prg = Expr<Int>.Quote(expand: { _ in .Literal(1) })

        let result = try eval0(prg, env: env)
        XCTAssertTrue(result.isVal(1))
    }

    /// Expand a quotation that refers to a value from the current scope: `x + 2`.
    func testQuote1() throws {
        let x = Name(id: 0)

        let env = Environment<Int>.bindOne(x, to: .Val(1))

        let prg = Expr<Int>.Quote(expand: { eval in
            let er = try eval(.Var(x))
            return try er.withVal { val in
                .Literal(val + 2)
            }
        })

        let result = try eval0(prg, env: env)
        XCTAssertTrue(result.isVal(3))
    }

    /// Match any positive int. No bindings.
    func testMatch() throws {
        let x = Name(id: 0)

        let prg = Expr<Int>.Match(expr: .Var(x), bindings: [], body: .Literal(1), otherwise: .Literal(0)) {
            try $0.withVal { val in
                if val > 0 {
                    return .Matched([])
                }
                else {
                    return .NoMatch
                }
            }
        }

        let positiveResult = try eval0(prg, env: Environment<Int>.bindOne(x, to: .Val(42)))
        XCTAssertTrue(positiveResult.isVal(1))

        let negativeResult = try eval0(prg, env: Environment<Int>.bindOne(x, to: .Val(-137)))
        XCTAssertTrue(negativeResult.isVal(0))
    }


// TODO: primitive recursion (factorial?)
// TODO: tail calls don't use stack


// TODO: lots of error cases that should throw a descriptive error and not just die or hang

    static var allTests = [
        ("testLiteral", testLiteral),
        ("testVar", testVar),
        ("testLet", testLet),
        ("testFn", testFn),
        ("testApp", testApp),
        ("testQuote", testQuote),
        ("testQuote1", testQuote1),
        ("testMatch", testMatch),
    ]
}
