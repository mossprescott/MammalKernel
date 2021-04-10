/// Node and attribute types defining the Mammal Kernel language, and mapping them
/// to the low-level AST for evaluation.
///
/// This is work is separated form the actual evaluator because it involves a lot of tedious
/// deconstruction of nodes. Putting it here keeps the evaluator itself simpler and easier to
/// test and debug.
///
/// Note: `Kernel` has no constructors; it's just a namespace for these declarations.
public enum Kernel {

    /// Evaluate/execute a program by translating it to the `Eval` AST, evaluating it, and
    /// then translating the resulting value back into a source-level representation.
    ///
    /// TODO: some kind of "environment" which maps additional nodes to expressions, as well
    /// as an actual `Environment` with values for eval0.
    public static func eval(_ program: Value) throws -> Value {
        let ast = translate(program)

        let result = try Eval.eval(ast, env: .Empty)

        return repr(result)
    }

// MARK: - Translate Nodes to Expr AST

    /// Translate a value made up of Nodes to an expression that can be evaluated. If the nodes do not represent a
    /// valid kernel program, an error is thrown, with minimal description. The intention is that a more complete
    /// validation is done prior to this being called, using a grammar over the nodes.
    ///
    /// Any extra attributes are ignored.
    ///
    /// Note: only Nodes with types defined in this file can be translated. A bare primitive value is never a valid program.
    ///
    /// If there are any structural errors, the resulting expression will fail (that is, produce an `Error` value
    /// when it's evaluated; if that portion of the program is never evaluated, no harm done.
    static func translate(_ program: Value) -> Eval.Expr<Value> {
        switch program {
        case .Prim(_):
            return .Fail(message: "Unexpected bare primitive: \(program)")

        case .Node(_, Nil.type, _):
            return .Literal(.Prim(.Nil))

        case .Node(let id, Bool_.type, let content):
            return requiredAttr(id, content, Bool_.value, expected: "Bool") { val in
                switch val {
                case .Prim(.Bool(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case .Node(let id, Int_.type, let content):
            return requiredAttr(id, content, Int_.value, expected: "Int") { val in
                switch val {
                case .Prim(.Int(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case .Node(let id, String_.type, let content):
            return requiredAttr(id, content, String_.value, expected: "String") { val in
                switch val {
                case .Prim(.String(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case .Node(let id, Var.type, _):
            return .Var(Eval.Name(id: id.id))

        case .Node(let id, Let.type, let content):
            let bindResult: Either<Eval.Expr<Value>, NodeId> =
                requiredAttr(id, content, Let.bind, expected: "Bind") { val in
                    switch val {
                    case .Node(let bindId, Bind.type, _):
                        return bindId
                    default:
                        return nil
                    }
                }
            switch bindResult {
            case .right(let bindId):
                let expr = requiredAttrToExpr(id, content, Let.expr, expected: "<Expr>", handle: translate)
                let body = requiredAttrToExpr(id, content, Let.body, expected: "<Expr>", handle: translate)
                return .Let(Eval.Name(id: bindId.id), expr: expr, body: body)
            case .left(let failExpr):
                return failExpr
            }

        case .Node(let id, Lambda.type, let content):
            fatalError("TODO")

        case .Node(let id, App.type, let content):
            fatalError("TODO")

        case .Node(let id, Quote.type, let content):
            fatalError("TODO")

        case .Node(let id, Match.type, let content):
            fatalError("TODO")

        case .Node(_, let type, _):
            return .Fail(message: "Unexpected node type: \(type.type)")
        }
    }

    /// Extract an attribute which is to translated directly to an `Expr`. If anything doesn't match, `Fail`.
    private static func requiredAttrToExpr(
        _ id: NodeId, _ node: NodeContent, _ attr: AttrName,
        expected: String,
        handle: (Value) -> Eval.Expr<Value>?)
    -> Eval.Expr<Value> {
        switch requiredAttr(id, node, attr, expected: expected, handle: handle) {
        case .right(let expr):
            return expr
        case .left(let expr):
            return expr
        }
    }

    /// Just defining a simple enum because `Result.Failure` is required to sub-type `Error`.
    private enum Either<Left, Right> {
        case left(Left)
        case right(Right)

        func merge() -> Left where Left == Right {
            switch self {
            case .left(let val): return val
            case .right(let val): return val
            }
        }
    }

    /// Extract an attribute which is to translated directly to an `Expr`. If anything doesn't match, `Fail`.
    private static func requiredAttr<T>(
        _ id: NodeId, _ node: NodeContent, _ attr: AttrName,
        expected: String,
        handle: (Value) -> T?)
    -> Either<Eval.Expr<Value>, T> {
        switch node {
        case .Attrs(let attrs):
            if let val = attrs[attr] {
                if let expr = handle(val) {
                    return .right(expr)
                }
                else {
                    return .left(.Fail(message: "Unexpected value for attribute \(attr) at node \(id): \(val)"))
                }
            }
            else {
                return .left(.Fail(message: "Missing required attribute \(attr) at node \(id)"))
            }
        default:
            return .left(.Fail(message: "Missing required attribute \(attr) at node \(id)"))
        }
    }

// MARK: - Quotation


// MARK: - Pattern matching


// MARK: - Reverse translation, aka "repr", aka "print"

    /// Convert the result of evaluation back to a value in the kernel. Wherever possible, the result is
    /// an expression that evaluates to the same value.
    /// Ordinary values are simply returned, but unevaluated functions and errors both get converted
    /// back to "expressions" that aren't very useful when evaluated.
    static func repr(_ value: Eval.Value<Value>) -> Value {
        switch value {
        case .Val(.Prim(let prim)):
            switch prim {
            case .Nil:
                return .Node(id: freshNodeId(),
                             type: Nil.type,
                             content: .Empty)

            case .Bool(_):
                return .Node(id: freshNodeId(),
                             type: Bool_.type,
                             content: .Attrs([Bool_.value: .Prim(prim)]))

            case .Int(_):
                return .Node(id: freshNodeId(),
                             type: Int_.type,
                             content: .Attrs([Int_.value: .Prim(prim)]))

            case .String(_):
                return .Node(id: freshNodeId(),
                             type: String_.type,
                             content: .Attrs([String_.value: .Prim(prim)]))
            }

        case .Val(.Node(_, _, _)):
            switch value {
            case .Val(let val):
                return val
            default:
                fatalError("doesn't happen")  // because we literally just matched it as a .Node
            }

        case .Fn(let arity, _):
            return .Node(id: freshNodeId(),
                         type: Fn.type,
                         content: .Attrs([
                            Fn.arity: .Prim(.Int(arity))
                         ]))

        case .Error(let err):
            return .Node(id: freshNodeId(),
                         type: Error.type,
                         content: .Attrs([
                            Error.description: .Prim(.String(String(describing: err)))
                         ]))
        }
    }

// MARK: - NodeId generation

    /// BOGUS: there might be a correct way to do this, but this sure ain't it.
    static func freshNodeId() -> NodeId {
        let id = nextId
        nextId = nextId + 1
        return NodeId(id)
    }
    private static var nextId: Swift.Int = 1000


// MARK: - Node types and attribute names

    public enum Nil {
        public static let type = NodeType("kernel", "nil")
    }
    public enum Bool_ {
        public static let type = NodeType("kernel", "bool")
        public static let value = AttrName(type, "value")
    }
    public enum Int_ {
        public static let type = NodeType("kernel", "int")
        public static let value = AttrName(type, "value")
    }
    public enum String_ {
        public static let type = NodeType("kernel", "string")
        public static let value = AttrName(type, "value")
    }

    public enum Var {
        public static let type = NodeType("kernel", "var")
        /// A Ref node pointing to the node that binds the variable.
        public static let ref = AttrName(type, "ref")
    }

    public enum Let {
        public static let type = NodeType("kernel", "let")
        public static let bind = AttrName(type, "bind")
        public static let expr = AttrName(type, "expr")
        public static let body = AttrName(type, "body")
    }

    /// Empty node which is the target for references to Let-bound variables and parameters.
    /// Or, can be Attrs containing other information not used during evaluation.
    public enum Bind {
        public static let type = NodeType("kernel", "bind")
        // TODO: use an optional parameter as a hint for the name during evaluation,
        // for debugging?
    }

    public enum Lambda {
        public static let type = NodeType("kernel", "lambda")
        public static let params = AttrName(type, "params")
        public static let body = AttrName(type, "body")
    }
    /// Elems containing Bind nodes.
    public enum Params {
        public static let type = NodeType("kernel", "params")
    }

    public enum App {
        public static let type = NodeType("kernel", "app")
        public static let fn = AttrName(type, "fn")
        public static let args = AttrName(type, "args")
    }
    /// Elems containing argument expressions.
    public enum Args {
        public static let type = NodeType("kernel", "args")
    }

    public enum Quote {
        public static let type = NodeType("kernel", "quote")
        /// Can be literally any Node. If it contains Unquote or UnquoteSplice nodes, they will
        /// be expanded.
        public static let body = AttrName(type, "body")
    }

    public enum Unquote {
        public static let type = NodeType("kernel", "unquote")
        public static let expr = AttrName(type, "expr")
        // TODO: levels?
    }

    public enum UnquoteSplice {
        public static let type = NodeType("kernel", "unquoteSplice")
        public static let expr = AttrName(type, "expr")
        // TODO: levels?
   }

    public enum Match {
        public static let type = NodeType("kernel", "match")
        public static let expr = AttrName(type, "expr")
        /// Can be literally any node. If it contains Bind nodes, they are the targets of
        /// variables bound inside `body`.
        public static let pattern = AttrName(type, "pattern")
        public static let body = AttrName(type, "body")
        public static let otherwise = AttrName(type, "otherwise")
   }

    /// TODO: does this need to be syntax? could be a function that you call, but that would require
    /// special translation.
    public enum Fail {
        public static let type = NodeType("kernel", "fail")
        public static let message = AttrName(type, "message")
    }

    public enum Fn {
        public static let type = NodeType("kernel", "fn")
        public static let arity = AttrName(type, "arity")
    }

    public enum Error {
        public static let type = NodeType("kernel", "error")
        public static let description = AttrName(type, "description")
    }
}
