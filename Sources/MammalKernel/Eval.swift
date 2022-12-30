/*
 Low-level types and machinery to define and evaluate programs, without specifiying
 the type of values that the programs manipulate.

 Hopefully this makes for a useful separation between the basic mechanics of interpreting
 programs (which is relatively fixed and trivial) and the more subtle business of
 encoding programs into a richer type which can encode both values and expressions.

 Fancy stuff:

 - Tail-call optimization: simple recursive programs consume no stack and no heap, which is
   necessary since we have no other way of doing iteration.

 - Monitoring: evaluation is halted after a pre-determined number of evaluation steps. This prevents
   a non-terminating program from making the system unusable, and should behave more predictably
   than, say, a clock-based timeout.

 Not yet implemented:

 - a better story on errors: should it be possible to trap and recover when (some kinds of)
   errors occur? Possibly. For now, any error just traps immediately without producing
   anything useful (except a hopefully helpful description of the problem.)

 - monitoring: put some (configurable) limits on computations, to prevent non-terminating
   programs (such as infinite loops and recursion) from making the system unusable. It
   should be possible to recover from that kind of error, and continue evaluating the rest
   of the program.

 - efficiency: haven't even thought about it, really.

 - run-time code generation:
   The evaluator defined here is a simple interpreter, however the idea is that the
   AST is simple enough to be compiled at runtime. The trick will be handling `Match` and
   `Quote` nodes, which have to call out to externally-defined implementations. If those
   features aren't needed, the translation should be quite simple.

 - "static", structural checks:
   Right now, everything that can go wrong gets trapped at runtime; there's no static
   validation of the structure of an expression before it's evaluated against the values
   in a particular environment. That may be unavoidable, though, because parts of the
   program can always be hidden inside `Quote` nodes that aren't determined until the
   values can be inspected. It's a meta-language!
   Still, if we can get some early warning or remove run-time checks by inspecting ASTs
   before starting evaluation, wouldn't that be cool?
 */

/// Namespace for types and functions for evaluation, working with any type of primitive values.
public enum Eval {

// MARK: - AST

    // Note: you can now use any Hashable type for names. Should we still provide some simple
    // wrapper like this?
//    /// A name identifies a storage location or other globally unique resource.
//    /// Somebody needs to take responsibility for generating them and keeping track.
//    public struct Name: Hashable {
//        var id: Int
//
//        /// TODO: hide this and/or add a mechanism to generate unique names
//        public init(id: Int) {
//            self.id = id
//        }
//    }

    /// AST for the (abstract) Mammel kernel language, consisting of *expressions* which can be *evaluated* to produce values of
    /// some type `T`.
    public indirect enum Expr<N: Hashable, T> {
        /// A fixed value.
        case Literal(_: T)

        /// Evaluates to a fixed value, which may be a runtime function or error, not just an ordinary value.
        ///
        /// HACK: it makes life easier for the Kernel translator to be able to inject `.Fn` values directly — without generating an
        /// id and injecting the value into the Environment — so for now this is helpful, but it feels like a wart when there's otherwise
        /// no need for the `Value` type to appear in the AST.
        ///
        /// If this is a legitimate need, then probably `Literal` should just take this kind of `Value`.
        case RuntimeLiteral(_: Value<N, T>)

        /// Looks up a value which has somehow been bound in the environment under a certain name.
        case Var(_: N)

        /// Evaluates some expression, stores the result under a name in a new scope, and
        /// then evaluates its `body` in that environment.
        case Let(_: N, expr: Expr, body: Expr)

        /// Constructs an unevaluated function, which can later be applied to some arguments via `App`.
        /// Note: if a name is provided, it is bound to the function itself when the body is evaluated,
        /// so that directly recursive functions can be defined.
        case Lambda(N?, params: [N], body: Expr)

        /// Applies a function to some arguments:
        /// - evaluate `fn` to a value, which must be an unevaluated function
        /// - check the number of arguments matches the function's parameters
        /// - evaluate each argument from left to right
        /// - make a new scope binding those values
        /// - evaluate the body of the function
        case App(fn: Expr, args: [Expr])

        /// Expands a partially-constructed value, which may involve sub-expressions which need to be
        /// evaluated in the current scope. 
        case Quote(expand: (EvalInContext<N, T>) throws -> Value<N, T>)

        /// Attempts to match the value of an expression against some externally-defined pattern. If the
        /// match succeeds, it produces a value for each binding, and `body` is evaluated with them in scope.
        /// If not, no new names are bound, and `otherwise` is evaluated instead.
        /// If the match succeeds but produces the wrong number of values, then an error is thrown, but if the match
        /// produces values in an unexpected order, then hilarity ensues.
        case Match(expr: Expr, bindings: [N], body: Expr, otherwise: Expr, match: MatchAndBind<N, T>)

        /// Construct an error value and yield it, interrupting most ongoing evaluation.
        case Fail(message: String)
    }

    /// Type for the function you use to look up values embedded in quotations. One of these is provided by the evaluator for your use.
    public typealias EvalInContext<N: Hashable, T> = (Expr<N, T>) throws -> Value<N, T>

    /// Type for helper functions used with `Expr.Match`. You provide one of these when you construct a Match expression.
    public typealias MatchAndBind<N: Hashable, T> = (Value<N, T>) throws -> MatchResult<N, T>

    /// Isomorphic to `[Value<T>]?`, but less confusing, especially when the result is `[]`.
    public enum MatchResult<N: Hashable, T> {
        case Matched([Value<N, T>])
        case NoMatch
    }


// MARK: - Runtime values

    /// A "run-time" value, which may be a simple value (of whatever kind of values can appear in programs), or something that exists
    /// only in memory and can't be represented directly as a program.
    ///
    /// Run-time-only values include:
    /// - unevaluated functions, which result from evaluating`Expr.Lambda` or may be "foreign" functions provided by the platform.
    /// - errors that occur during evaluation, which come with extra information to aid debugging
    public indirect enum Value<N: Hashable, T> {
        case Val(T)

        /// TODO: optional `Expr` which was used to define the function (but which might contain free vars.)
        case Fn(arity: Int, body: ([Value]) throws -> Value)

        /// These values can arise from error conditions during evaluation, or can be emitted directly using `Expr.Fail`.
        case Error(RuntimeError<N, T>)

        /// True if this is a simple value equal to the given. There's no simple way to compare Fn values.
        public func isVal(_ val: T) -> Bool where T: Equatable {
            switch self {
            case .Val(let v):
                return v == val

            case .Fn(_, _), .Error(_):
                return false
            }
        }

        /// Consume a value that's expected _not_ to be a function or error. If it is, an error is thrown.
        public func withVal<U>(handle: (T) throws -> U) throws -> U {
            switch self {
            case .Val(let val):
                return try handle(val)
            case .Fn(_, _), .Error(_):
                throw RuntimeError<N, T>.TypeError(expected: ".Val", found: self)
            }
        }

        /// Consume a value that's expected to be a function. If it's not, an error is thrown. Note: it's up to the
        /// caller to check the arity.
        public func withFn<U>(handle: (Int, ([Value]) throws -> Value) throws -> U) throws -> U {
            switch self {
            case .Fn(let arity, let fn):
                return try handle(arity, fn)
            case .Val(_), .Error(_):
                throw RuntimeError<N, T>.TypeError(expected: ".Fn", found: self)
            }
        }

        /// Consume a value that's expected to be an error. If it's not, an error is thrown.
        public func withError<U>(handle: (RuntimeError<N, T>) throws -> U) throws -> U {
            switch self {
            case .Error(let re):
                return try handle(re)
            case .Val(_), .Fn(_, _):
                throw RuntimeError<N, T>.TypeError(expected: ".Error", found: self)
            }
        }

        /// Rewrite any name(s) that might be embedded in an error value.
        func mapName<NN>(_ iso: Iso<N, NN>) -> Value<NN, T> {
            switch self {
            // TODO: Unsafe casts?
            case .Val(let val): return .Val(val)
            case .Fn(let a, let b):
                func g(args: [Value<NN, T>]) throws -> Value<NN, T> {
                    let args1: [Value<N, T>] = args.map  { $0.mapName(iso.inverse) }
                    return try b(args1).mapName(iso)
                }
                return .Fn(arity: a, body: g)
            case .Error(let e): return .Error(e.mapName(iso))
            }
        }
    }

    /// Errors in the structure of Expressions (i.e. "static" errors.)
    /// Given there's no type system, are there static errors? Examples might be things like bad nesting of quotations and references
    /// variables that aren't in scope.
    public enum EvaluationError: Error {
        // TODO: ?
    }

    /// Errors that can occur during evaluation of a statically valid program.
    public enum RuntimeError<N: Hashable, T>: Error {
        /// For example, a function was expected and some other value was found.
        case TypeError(expected: String, found: Value<N, T>)

        /// Tried to apply a function to the wrong number of arguments.
        case ArityError(expected: Int, found: [Value<N, T>])

        /// Tried to refer to a bound variable, and it wasn't found in the current scope.
        /// This is actually a "static" error, probably, depending on the semantics…
        case NotBound(_: N)

        /// Evaluation made more recursive calls than the runtime could handle.
        case StackOverflow

        /// Evaluation went on too long without producing a result, and was killed to avoid hanging the system.
        case TimeOut

        /// An error produced intentionally by user code (via `Expr.Fail`).
        /// Need a better name.
        case UserError(message: String)

        /// For any unanticipated need.
        case OtherError(message: String)

        func mapName<NN>(_ iso: Iso<N, NN>) -> RuntimeError<NN, T> {
            switch self {
            case .NotBound(let name):
                return .NotBound(iso.map(name))
            case .TypeError(let exp, let found):
                return .TypeError(expected: exp, found: found.mapName(iso))
            case .ArityError(let exp, let found):
                return .ArityError(expected: exp, found: found.map { $0.mapName(iso) })

 // TODO: unsafe casts?
            case .StackOverflow:
                return .StackOverflow
            case .TimeOut:
                return .TimeOut
            case .UserError(let msg):
                return .UserError(message: msg)
            case .OtherError(let msg):
                return .OtherError(message: msg)
            }
        }
    }


// MARK: - Evaluation

    ///// Evaluate with a set of bindings, then wrap the result in an Expr, possibly discarding a non-representable value.
    ///// TODO: it's probably not possible to map values back to expressions without some knowledge of the underlying
    ///// type, so this will be implemented elsewhere.
    //public func eval<T>(_ node: Expr<T>, env: Environment<T>) throws -> Expr<T> {
    //    let result = try eval0(node, env: env)
    //    fatalError("TODO")
    //}

    /// Evaluate an expression, producing a value (which may be an error.)
    ///
    /// TODO: not public, if nobody needs to use it directly (except possibly tests)
    ///
    /// - Parameters:
    ///   - budget: maximum "steps" of evaluation that should be performed before giving up and yielding a TimeOut
    ///
    /// - Returns: the result value, if evaluation completes normally;
    ///   `RuntimeError.TimeOut` if evaluation isn't finished after `budget` steps
    static public func eval<N: Hashable, T>(_ node: Expr<N, T>, env externalEnv: [N: Value<N, T>], budget: Int = 1000) -> Value<N, T> {
        typealias Env = SimpleTrie<Value<UInt, T>>

        func with(_ env: Env, _ name: UInt, boundTo val: Value<UInt, T>) -> Env {
            var newEnv = env
            newEnv[name] = val
            return newEnv
        }

        func with(_ env: Env, names: [UInt], boundTo vals: [Value<UInt, T>]) throws -> Env {
            guard names.count == vals.count else {
                throw RuntimeError.ArityError(expected: names.count, found: vals)
            }
            return zip(names, vals).reduce(env) { (e, t) in
                with(e, t.0, boundTo: t.1)
            }
        }

        var steps = 0

        /// Inner evaluation loop.
        func go(_ startNode: Expr<UInt, T>, env startEnv: Env) throws -> Value<UInt, T> {
            var node = startNode
            var env = startEnv

            while true {
                if steps >= budget {
                    throw RuntimeError<N, T>.TimeOut
                }
                steps += 1

//                var nextNode: Node
//                var nextEnv: Env

                switch node {
                case .Literal(let val):
                    return .Val(val)

                case .RuntimeLiteral(let val):
                    return val

                case .Var(let name):
                    if let val = env[name] {
                        return val
                    }
                    else {
                        throw RuntimeError<UInt, T>.NotBound(name)
                    }

                case .Let(let name, let expr, let body):
                    let value = try go(expr, env: env)
                    let newEnv = with(env, name, boundTo: value)
                    return try go(body, env: newEnv)

                case .Lambda(let name, let params, let body):
                    func f(args: [Value<UInt, T>]) throws -> Value<UInt, T> {
                        var newEnv = try with(env, names: params, boundTo: args)
                        if let n = name {
                            // Yikes: this is one way to get the lambda's name bound when it's
                            // evaluated, but it ain't pretty. Is there a more natural way to
                            // close this loop?
                            newEnv = with(newEnv, n, boundTo: .Fn(arity: params.count, body: f))
                        }
                        return try go(body, env: newEnv)
                    }
                    return .Fn(arity: params.count, body: f)

                case .App(let fn, let args):
                    return try go(fn, env: env).withFn { (arity, f) in
                        let argVs = try args.map { try go($0, env: env) }
                        guard args.count == arity else {
                            throw RuntimeError.ArityError(expected: arity, found: argVs)
                        }
                        return try f(argVs)
                    }

                case .Quote(let expand):
                    return try expand { expr in
                        try go(expr, env: env)
                    }

                case .Match(let expr, let bindings, let body, let otherwise, let match):
                    let value = try go(expr, env: env)
                    switch try match(value) {
                    case .Matched(let boundValues):
                        let newEnv = try with(env, names: bindings, boundTo: boundValues)
                        return try go(body, env: newEnv)
                    case .NoMatch:
                        return try go(otherwise, env: env)
                    }

                case .Fail(let msg):
                    return Value.Error(.UserError(message: msg))
                }
            }
        }

        // Re-assign names for fast binding/lookup:
        // Uh, so, this doesn't work for interesting cases because more nodes can be produced
        // during evaluation (e.g. when unquotes are expanded). We'll have to be able to add new ids
        // to the mapping as we encounter them.
        // Also, names from the provided environment might not be referenced until later, so they
        // also need to get added to the mapping from the start.
        // Given all that, would it be simpler just to use a proper (hash-)map and not try to do
        // any of this rewriting jazz?
        let idxToName: [N] = Array(node.collectNames()) + externalEnv.keys
        let nameToIdx = Dictionary<N, UInt>(idxToName.enumerated().map { (idx, name) in (name, UInt(idx)) },
                                            uniquingKeysWith: { (k1, k2) in k1 })
        let nameIso: Iso<N, UInt> = Iso(
            map: { nameToIdx[$0]! },
            unmap: { idx in idxToName[Int(idx)] })

        var env: Env = SimpleTrie()
        for (n, v) in externalEnv {
            env[nameToIdx[n]!] = v.mapName(nameIso)
        }

        let renamedRoot: Expr<UInt, T> = node.mapNames(nameIso)

        do {
            let result = try go(renamedRoot, env: env)
            return result.mapName(nameIso.inverse)
        }
        catch let re as RuntimeError<UInt, T> {
            // Wrap the captured error as a value:
            return .Error(re.mapName(nameIso.inverse))
        }
        catch let error {
            return .Error(.OtherError(message: "Unexpected error: \(error)"))
        }
    }

//    /// Private wrapper for RuntimeErrors so we can use exception handling to capture them. Outside of this function, they're just
//    /// ordinary values.
//    fileprivate struct ThrowableRuntimeError<T>: Error {
//        var error: RuntimeError<T>
//    }
}

/// A random FP concept that leaked in: a bi-directional mapping from one type to another. To be useful, the mapping should be
/// one-to-one, but practically speaking we'll use it with types that have some values we don't care about, so if either function ever fails
/// it will have to do so fatally.
struct Iso<A, B> {
    /// aka `view`
    var map: (A) -> B
    /// aka `review`
    var unmap: (B) -> A

    var inverse: Iso<B, A> {
        Iso<B, A>(map: unmap, unmap: map)
    }
}

extension Eval.Expr {
    func collectNames() -> Set<N> {
        var names: Set<N> = Set()

        func go(_ expr: Eval.Expr<N, T>) {
            switch expr {
            case .Literal(_):
                break
            case .RuntimeLiteral(_):
                // TODO: can names be buried in, say, .Fn, in a way we could deal with here?
                break
            case .Var(let target):
                names.insert(target)
            case .Let(let name, expr: let expr, body: let body):
                names.insert(name)
                go(expr)
                go(body)
            case .Lambda(let name, params: let params, body: let body):
                if let name = name {
                    names.insert(name)
                }
                params.forEach { names.insert($0) }
                go(body)
            case .App(fn: let fn, args: let args):
                go(fn)
                args.forEach(go)
            case .Quote(expand: _):
                // TODO: somehow defer and capture/translate names on both sides of embedded expressions?
                break
            case .Match(expr: let expr, bindings: let bindings, body: let body, otherwise: let otherwise, match: _):
                go(expr)
                bindings.forEach { names.insert($0) }
                go(body)
                go(otherwise)
                // TODO: maybe nothing here because values only go into the match function?
                // something(match)
            case .Fail(message: _):
                break
            }
        }

        go(self)

        return names
    }

    func mapNames<NN>(_ iso: Iso<N, NN>) -> Eval.Expr<NN, T> {
        func go(_ expr: Eval.Expr<N, T>) -> Eval.Expr<NN, T> {
            switch expr {
            case .Literal(let val):
                return .Literal(val)
            case .RuntimeLiteral(let val):
                return .RuntimeLiteral(val.mapName(iso))
            case .Var(let target):
                return .Var(iso.map(target))
            case .Let(let name, expr: let expr, body: let body):
                return .Let(iso.map(name), expr: go(expr), body: go(body))
            case .Lambda(let name, params: let params, body: let body):
                return .Lambda(name.map(iso.map), params: params.map(iso.map), body: go(body))
            case .App(fn: let fn, args: let args):
                return .App(fn: go(fn), args: args.map(go))
            case .Quote(expand: let expand):
                // TODO: some of these lookups are going to fail...
                func expand1(_ f: (Eval.Expr<NN, T>) throws -> Eval.Value<NN, T>) throws -> Eval.Value<NN, T> {
                    func g(_ expr: Eval.Expr<N, T>) throws -> Eval.Value<N, T> {
                        return try f(go(expr)).mapName(iso.inverse)
                    }
                    return try expand(g).mapName(iso)
                }
                return .Quote(expand: expand1)
            case .Match(expr: let expr, bindings: let bindings, body: let body, otherwise: let otherwise, match: let match):
                func match1(_ val: Eval.Value<NN, T>) throws -> Eval.MatchResult<NN, T> {
                    let val1 = val.mapName(iso.inverse)
                    switch try match(val1) {
                    case .Matched(let vals):
                        return .Matched(vals.map { $0.mapName(iso) })
                    case .NoMatch:
                        return .NoMatch
                    }
                }
                return .Match(expr: go(expr), bindings: bindings.map(iso.map), body: go(body), otherwise: go(otherwise), match: match1)
            case .Fail(message: let msg):
                return .Fail(message: msg)
            }
        }

        return go(self)
    }
}
