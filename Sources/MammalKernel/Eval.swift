/*
 Low-level types and machinery to define and evaluate programs, without specifiying
 the type of values that the programs manipulate.

 Hopefully this makes for a useful separation between the basic mechanics of interpreting
 programs (which is relatively fixed and trivial) and the more subtle business of
 encoding programs into a richer type which can encode both values and expressions.

 Not yet implemented:

 - a better story on errors: should it be possible to trap and recover when (some kinds of)
   errors occur? Possibly. For now, any error just traps immediately without producing
   anything useful (except a hopefully helpful description of the problem.)

 - monitoring: put some (configurable) limits on computations, to prevent non-terminating
   programs (such as infinite loops and recursion) from making the system unusable. It
   should be possible to recover from that kind of error, and continue evaluating the rest
   of the program.

 - efficiency: haven't even thought about it, really. Is it bad that every variable
   reference is a linear lookup? Probably, yes.

 - tail-calls: this language has no iteration mechasism, so we need to be able to
   use recursion instead. It will be fun embedding that in Swift (probably using
   iteration.)

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

/// Namespace for types and functions for evaluation.
public enum Eval {

// MARK: - AST

    /// A name identifies a storage location or other globally unique resource.
    /// Somebody needs to take responsibility for generating them and keeping track.
    public struct Name: Hashable {
        var id: Int

        /// TODO: hide this and/or add a mechanism to generate unique names
        public init(id: Int) {
            self.id = id
        }
    }

    /// AST for the Mammel kernel language, consisting of expressions which can be evaluated to produce values of some type `T`.
    public indirect enum Expr<T> {
        /// Evaluates to a fixed value.
        case Literal(_: T)

        /// Evaluates to a fixed value, which may be a runtime function or error, not just an ordinary value.
        ///
        /// HACK: it makes life easier for the Kernel translator to be able to inject `.Fn` values directly — without generating an
        /// id and injecting the value into the Environment — so for now this is helpful, but it feels like a wart when there's otherwise
        /// no need for the `Value` type to appear in the AST.
        ///
        /// If this is a legitimate need, then probably `Literal` should just take this kind of `Value`.
        case RuntimeLiteral(_: Value<T>)

        /// Looks up a value which has somehow been bound in the environment under a certain name.
        case Var(_: Name)

        /// Evaluates some expression, stores the result under a name in a new scope, and
        /// then evaluates its `body` in that environment.
        case Let(_: Name, expr: Expr, body: Expr)

        /// Constructs an unevaluated function, which can later be applied to some arguments via `App`.
        /// Note: if a name is provided, it is bound to the function itself when the body is evaluated,
        /// so that directly recursive functions can be defined.
        case Lambda(Name?, params: [Name], body: Expr)

        /// Applies a function to some arguments:
        /// - evaluate `fn` to a value, which must be an unevaluated function
        /// - check the number of arguments matches the function's parameters
        /// - evaluate each argument from left to right
        /// - make a new scope binding those values
        /// - evaluate the body of the function
        case App(fn: Expr, args: [Expr])

        /// Expands a partially-constructed value, which may involve sub-expressions which need to be
        /// evaluated in the current scope. 
        case Quote(expand: (EvalInContext<T>) throws -> Value<T>)

        /// Attempts to match the value of an expression against some externally-defined pattern. If the
        /// match succeeds, it produces a value for each binding, and `body` is evaluated with them in scope.
        /// If not, no new names are bound, and `otherwise` is evaluated instead.
        /// If the match succeeds but produces the wrong number of values, then an error is thrown, but if the match
        /// produces values in an unexpected order, then hilarity ensues.
        case Match(expr: Expr, bindings: [Name], body: Expr, otherwise: Expr, match: MatchAndBind<T>)

        /// Construct an error value and yield it, interrupting most ongoing evaluation.
        case Fail(message: String)
    }

    /// Type for the function you use to look up values embedded in quotations. One of these is provided by the evaluator for your use.
    public typealias EvalInContext<T> = (Expr<T>) throws -> Value<T>

    /// Type for helper functions used with `Expr.Match`. You provide one of these when you construct a Match expression.
    public typealias MatchAndBind<T> = (Value<T>) throws -> MatchResult<T>

    /// Isomorphic to `[Value<T>]?`, but less confusing, especially when the result is `[]`.
    public enum MatchResult<T> {
        case Matched([Value<T>])
        case NoMatch
    }


// MARK: - Runtime values

    /// A "run-time" value can be either a simple value or an unevaluated function, which may have been defined
    /// as an `Expr.Lambda` or may be a "foreign" function provided by the platform.
    public indirect enum Value<T> {
        case Val(T)

        /// TODO: optional `Expr` which was used to define the function (but which might contain free vars.)
        case Fn(arity: Int, body: ([Value]) throws -> Value)

        /// These values can arise from error conditions during evaluation, or can be emitted directly using `Expr.Fail`.
        case Error(RuntimeError<T>)

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
                throw RuntimeError.TypeError(expected: ".Val", found: self)
            }
        }

        /// Consume a value that's expected to be a function. If it's not, an error is thrown. Note: it's up to the
        /// caller to check the arity.
        public func withFn<U>(handle: (Int, ([Value]) throws -> Value) throws -> U) throws -> U {
            switch self {
            case .Val(_), .Error(_):
                throw RuntimeError.TypeError(expected: ".Fn", found: self)
            case .Fn(let arity, let fn):
                return try handle(arity, fn)
            }
        }
    }

    /// Errors in the structure of Expressions (i.e. "static" errors.)
    /// Given there's no type system, are there static errors?
    public enum EvaluationError: Error {
        // TODO: ?
    }

    /// Errors that can occur during evaluation of a statically valid program.
    public enum RuntimeError<T>: Error {
        /// For example, a function was expected and some other value was found.
        case TypeError(expected: String, found: Value<T>)

        /// Tried to apply a function to the wrong number of arguments.
        case ArityError(expected: Int, found: [Value<T>])

        /// Tried to refer to a bound variable, and it wasn't found in the current scope.
        /// This is actually a "static" error, probably, depending on the semantics…
        case NotBound(_: Name)

        /// An error produced intentionally by user code (via `Expr.Fail`).
        /// Need a better name.
        case UserError(message: String)
    }

// MARK: - Environment/Scope

    public indirect enum Environment<T> {
        case Empty
        case Scope(name: Name, value: Value<T>, parent: Environment<T>)

        /// New scope with exactly one bound name. Meant to read better than `.Empty.with(...)`.
        public static func bindOne<T>(_ name: Name, to val: Value<T>) -> Environment<T> {
            return .Scope(name: name, value: val, parent: .Empty)
        }

        public func with(_ name: Name, boundTo val: Value<T>) -> Environment<T> {
            .Scope(name: name, value: val, parent: self)
        }

        /// A common pattern is to bind a series of names, for example function arguments. Here, check that
        /// the two lists have the same length and call it an arity errror if not. Note: this probably makes it too easy
        /// to mix up the order and cause confusion, but there's basically no way to enforce that short of a type
        /// system.
        public func with(names: [Name], boundTo vals: [Value<T>]) throws -> Environment<T> {
            guard names.count == vals.count else {
                throw RuntimeError.ArityError(expected: names.count, found: vals)
            }
            return zip(names, vals).reduce(self) { (e, t) in
                e.with(t.0, boundTo: t.1)
            }
        }

        public func lookup(_ name: Name) -> Value<T>? {
            switch self {
            case .Empty:
                return nil

            case .Scope(name, let val, _):
                return val

            case .Scope(_, _, let parent):
                return parent.lookup(name)
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

    /// TODO: not public, if nobody needs to use it directly (except possibly tests)
    static public func eval<T>(_ node: Expr<T>, env: Environment<T>) throws -> Value<T> {
        switch node {
        case .Literal(let val):
            return .Val(val)

        case .RuntimeLiteral(let val):
            return val

        case .Var(let name):
            if let val = env.lookup(name) {
                return val
            }
            else {
                throw RuntimeError<T>.NotBound(name)
            }

        case .Let(let name, let expr, let body):
            let value = try eval(expr, env: env)
            let newEnv = env.with(name, boundTo: value)
            return try eval(body, env: newEnv)

        case .Lambda(let name, let params, let body):
            func f(args: [Value<T>]) throws -> Value<T> {
                var newEnv = try env.with(names: params, boundTo: args)
                if let n = name {
                    // Yikes: this is one way to get the lambda's name bound when it's
                    // evaluated, but it ain't pretty. Is there a more natural way to
                    // close this loop?
                    newEnv = newEnv.with(n, boundTo: .Fn(arity: params.count, body: f))
                }
                return try eval(body, env: newEnv)
            }
            return .Fn(arity: params.count, body: f)

        case .App(let fn, let args):
            return try eval(fn, env: env).withFn { (arity, f) in
                let argVs = try args.map { try eval($0, env: env) }
                guard args.count == arity else {
                    throw RuntimeError.ArityError(expected: arity, found: argVs)
                }
                return try f(argVs)
            }

        case .Quote(let expand):
            return try expand { expr in
                try eval(expr, env: env)
            }

        case .Match(let expr, let bindings, let body, let otherwise, let match):
            let value = try eval(expr, env: env)
            switch try match(value) {
            case .Matched(let boundValues):
                let newEnv = try env.with(names: bindings, boundTo: boundValues)
                return try eval(body, env: newEnv)
            case .NoMatch:
                return try eval(otherwise, env: env)
            }

        case .Fail(let msg):
            return Value.Error(.UserError(message: msg))
        }
    }
}

// MARK: - Debugging aids

extension Eval.Environment: CustomDebugStringConvertible {
    public var debugDescription: String {
        switch self {
        case .Empty:
            return "<empty>"
        case .Scope(let name, let value, let parent):
            return parent.debugDescription + "\n\(name.id) = \(value)"
        }
    }
}
