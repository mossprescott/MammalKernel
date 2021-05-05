/// Node and attribute types defining the Mammal Kernel language, and mapping them
/// to the low-level AST for evaluation.
///
/// This is work is separated from the actual evaluator because it involves a lot of tedious
/// deconstruction of nodes. Putting it here keeps the evaluator itself simpler and easier to
/// test and debug.
///
/// Note: `Kernel` has no constructors; it's just a namespace for these declarations.
public enum Kernel {

    /// Evaluate/execute a program by translating it to the `Eval` AST, evaluating it, and
    /// then translating the resulting value back into a source-level representation.
    ///
    /// - Parameter constants: A mapping giving additional node types, each of which is to be translated to
    /// the provided literal value. This provides a mechanism to resolve references to values provided by the platform,
    /// for example built-in functions.
    public static func eval(_ program: Node, constants: [NodeType: Eval.Value<Node.Value>]) throws -> Node {
        let ast = translate(program, constants: constants)

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
    static func translate(_ node: Node, constants: [NodeType: Eval.Value<Node.Value>]) -> Eval.Expr<Node.Value> {
        let translateChild = { translate($0, constants: constants) }

        switch node.type {
        case Nil.type:
            return .Literal(.Prim(.Nil))

        case Bool_.type:
            return required(node, Bool_.value, expected: "Bool") { val in
                switch val {
                case .Prim(.Bool(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case Int_.type:
            return required(node, Int_.value, expected: "Int") { val in
                switch val {
                case .Prim(.Int(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case String_.type:
            return required(node, String_.value, expected: "String") { val in
                switch val {
                case .Prim(.String(_)): return .Literal(val)
                default: return nil
                }
            }.merge()

        case Var.type:
            switch node.content {
            case .Ref(let target):
                return .Var(Eval.Name(id: target.id)) // TODO: look under "ref"

            default:
                return .Fail(message: "Var is not a Ref at node \(node.id)")
            }

        case Let.type:
            let bindResult: Either<Eval.Expr<Node.Value>, NodeId> =
                required(node, Let.bind, expected: "Bind") { val in
                    switch val {
                    case .Node(let bindNode) where bindNode.type == Bind.type:
                        return bindNode.id
                    default:
                        return nil
                    }
                }
            switch bindResult {
            case .right(let bindId):
                let expr = requiredExpr(node, Let.expr, handle: translateChild)
                let body = requiredExpr(node, Let.body, handle: translateChild)
                return .Let(Eval.Name(id: bindId.id), expr: expr, body: body)
            case .left(let failExpr):
                return failExpr
            }

        case Lambda.type:
            let params = required(node, Kernel.Lambda.params, expected: "<Binds>") { val -> [Node]? in
                switch val {
                case .Node(let paramsNode):
                    switch paramsNode.content {
                    case .Elems(let paramsChildren):
                        return paramsChildren
                    default:
                        return nil
                    }
                default:
                    return nil
                }
            }
            switch params {
            case .left(let err):
                return err
            case .right(let paramNodes):
                let body = requiredExpr(node, Kernel.Lambda.body, handle: translateChild)
                return .Lambda(Eval.Name(id: node.id.id),
                               params: paramNodes.map { n in Eval.Name(id: n.id.id) },
                               body: body)
            }

        case App.type:
            let fn = requiredExpr(node, Kernel.App.fn, handle: translateChild)
            let args = required(node, Kernel.App.args, expected: "<Exprs>") { val -> [Node]? in
                switch val {
                case .Node(let argsNode):
                    switch argsNode.content {
                    case .Elems(let argsChildren):
                        return argsChildren
                    default: return nil
                    }
                default:
                    return nil
                }

            }
            switch args {
            case .left(let err):
                return err
            case .right(let argNodes):
                return .App(fn: fn, args: argNodes.map { translate($0, constants: constants) })
            }

        case Quote.type:
            return requiredExpr(node, Quote.body) { node in
                .Quote { try expandQuotedNode(node, constants: constants, eval: $0) }
            }

        case Unquote.type, UnquoteSplice.type:
            return .Fail(message: "Unquote without enclosing Quote at node \(node.id)")

        case Match.type:
            let expr = requiredExpr(node, Match.expr, handle: translateChild)
            let body = requiredExpr(node, Match.body, handle: translateChild)
            let otherwise = requiredExpr(node, Match.otherwise, handle: translateChild)

            // The pattern is always interpreted as if quoted. It is not evaluated in the
            // usual way, but only compared to the value of expr.
            // TODO: what if an unquoted expression is embedded? There seems to be no
            // compelling reason not to evaluate it and then match against it. Well, except that
            // it could get complicated.
            switch required(node, Match.pattern, expected: "<pattern>", handle: { val -> Node? in
                switch val {
                case .Node(let node): return node
                default: return nil
                }
            }) {
            case .left(let err):
                return err

            case .right(let pattern):
                let bindings = findBindings(inPattern: pattern)

                return .Match(expr: expr,
                              bindings: bindings.map { Eval.Name(id: $0.id) },
                              body: body,
                              otherwise: otherwise) { rv in
                                switch rv {
                                case .Val(let val):
                                    return matchAndBind(pattern: pattern,
                                                        withValue: val)
                                default:
                                    return .NoMatch
                                }
                              }
            }

        default:
            if let val = constants[node.type] {
                return .RuntimeLiteral(val)
            }
            else {
                return .Fail(message: "Unexpected node type: \(node.type)")
            }
        }
    }

    /// Extract an attribute which is to be translated in some way. If it's missing, or if the translation fails, then `Fail`.
    private static func required<T>(
        _ node: Node, _ attr: AttrName,
        expected: String, handle: (Node.Value) -> T?
    ) -> Either<Eval.Expr<Node.Value>, T> {
        requiredAttr(node, attr, expected: expected, handle: handle)
            .mapLeft { .Fail(message: $0) }
    }

    /// Extract an attribute which is to be translated directly to an `Expr`. If anything doesn't match, `Fail`.
    private static func requiredExpr(
        _ node: Node, _ attr: AttrName,
        handle: (Node) -> Eval.Expr<Node.Value>?
    )  -> Eval.Expr<Node.Value> {
        required(node, attr, expected: "<Expr>") { val -> Eval.Expr<Node.Value>? in
            switch val {
            case .Prim(_):
                return nil
            case .Node(let node):
                return handle(node)
            }
        }.merge()
    }


// MARK: - Quotation

    /// Expand a node which appeared inside a `Quote` node's body, in a particular environment during evaluation.
    /// The structure of the node is inspected: if it is an `Unquote`, its `expr` is evaluated and substituted. Otherwise any child
    /// nodes are subjected to the same scrutiny.
    ///
    /// Note: this function gets called during evaluation, and in turn calls back into the evaluator,
    /// so it can throw RuntimeError, in keeping with the way those errors are currently handled.
    static func expandQuotedNode(_ root: Node,
                                 constants: [NodeType: Eval.Value<Node.Value>],
                                 eval: Eval.EvalInContext<Node.Value>)
                                        throws -> Eval.Value<Node.Value> {

        func expandNode(_ node: Node, level: Int) throws -> Eval.Value<Node.Value> {
            if node.type == Unquote.type, let expr = try unrollUnquoteExpr(node, levels: level) {
                return try eval(translate(expr, constants: constants))
            }
            else if level == 1 && node.type == UnquoteSplice.type {
                return .Error(.TypeError(expected: "something other than UnquoteSplice", found: .Val(.Node(node))))
            }
            else {
                let innerLevel: Int = {
                    switch node.type {
                    case Kernel.Quote.type:
                        return level + 1
                    case Kernel.Unquote.type, Kernel.UnquoteSplice.type:
                        return level - 1
                    default:
                        return level
                    }
                }()

                switch node.content {
                case .Attrs(let attrs):
                    let expandedAttrs = try attrs.mapValues { val throws -> Node.Value in
                        switch val {
                        case .Prim(_): return val
                        case .Node(let child): return try expandChild(child, level: innerLevel)
                        }
                    }
                    return .Val(.Node(Node(node.id, node.type, .Attrs(expandedAttrs))))

                case .Elems(let elems):
                    let expandedElems = try elems.flatMap { child throws -> [Node] in
                        let expanded = try expandChildOrSplice(child, level: innerLevel)
                        return try expanded.map { val -> Node in
                            switch val {
                            case .Prim(let expandedVal):
                                throw Eval.RuntimeError<Node.Value>.TypeError(
                                    expected: "node for Elems during quote expansion",
                                    found: .Val(.Prim(expandedVal)))
                            case .Node(let expandedChild):
                                return expandedChild
                            }
                        }
                    }
                    return .Val(.Node(Node(node.id, node.type, .Elems(expandedElems))))

                case .Ref(_):
                    // TODO: verify that the ref points to a node within the quotation?
                    // TODO: re-label nodes to avoid collisions
                    return .Val(.Node(node))

                case .Empty:
                    return .Val(.Node(node))
                }
            }
        }

        func expandChild(_ child: Node, level: Int) throws -> Node.Value {
            let expandedVal = try expandNode(child, level: level)
            switch expandedVal {
            case .Val(let expandedChild):
                return expandedChild
            case .Fn(_, _):
                throw Eval.RuntimeError.TypeError(expected: "normal value during quote expansion",
                                                  found: expandedVal)
            case .Error(let err):
                throw err
            }
        }

        func expandChildOrSplice(_ child: Node, level: Int) throws -> [Node.Value] {
            if level == 1 && child.type == Kernel.UnquoteSplice.type {
                let expanded = try eval(requiredExpr(child, UnquoteSplice.expr) { expr in
                    translate(expr, constants: constants)
                })
                switch expanded {
                case .Val(.Node(let node)):
                    switch node.content {
                    case .Elems(let elems):
                        return elems.map { .Node($0) }
                    default:
                        throw Eval.RuntimeError.TypeError(expected: "sequence node",
                                                          found: .Val(expanded))
                    }
                default:
                    throw Eval.RuntimeError.TypeError(expected: "sequence node",
                                                      found: .Val(expanded))
                }
            }
            else {
                let expanded = try expandChild(child, level: level)
                return [expanded]
            }
        }

        // Dig into `unquote` nodes to find the `expr` node embedded exactly the given number of
        // levels, if any. If any other node is encountered first, then nil. If an unquote is found
        // first, but it doesn't have a node for `expr`, then throw.
        func unrollUnquoteExpr(_ node: Node, levels: Int) throws -> Node? {
            let result: Either<Eval.Expr<Node.Value>, Node> = required(node, Unquote.expr, expected: "<node>") { expr in
                switch expr {
                case .Prim(_):
                    return nil
                case .Node(let exprNode):
                    return exprNode
                }
            }
            switch result {
            case .left(let err):
                let _ = try eval(err)
                fatalError("can't get here")

            case .right(let exprNode):
                if levels == 1 {
                    return exprNode
                }
                else if exprNode.type == Kernel.Unquote.type {
                    return try unrollUnquoteExpr(exprNode, levels: levels-1)
                }
                else {
                    return nil
                }
            }
        }

        return try expandNode(root, level: 1)
    }

// MARK: - Pattern matching

    /// Search a node which appears as the pattern in a Match expression. For the time being, the bindings are just any Bind nodes.
    /// No special handling of Quote and Ref nodes, which is very possibly wrong.
    static func findBindings(inPattern node: Node) -> [NodeId] {
        if node.type == Kernel.Bind.type {
            return [node.id]
        }
        else {
            switch node.content {
            case .Attrs(let attrs):
                return attrs.values.flatMap { val -> [NodeId] in
                    switch val {
                    case .Prim(_):
                        return []
                    case .Node(let child):
                        return findBindings(inPattern: child)
                    }
                }

            case .Elems(let elems):
                return elems.flatMap(findBindings)

            case .Ref(_):
                return []

            case .Empty:
                return []
            }
        }
    }

    /// Compare the node which appears as the pattern in a Match expression with a value that arises at runtime. If the value is a
    /// match, then also extract the value for each binding that appears in the pattern.
    static func matchAndBind(pattern: Node, withValue runtimeValue: Node.Value) -> Eval.MatchResult<Node.Value> {
        // Convert back to source representation:
        let rtNode = repr(.Val(runtimeValue))

        // Find the bindings again, so we can assemble values in the same order.
        // Note: this assumes findBindings is deterministic.
        let bindings = findBindings(inPattern: pattern)

        func loop(patternNode: Node, valueNode: Node) -> [NodeId: Node.Value]? {
            // This will handle the case of a bind at the root:
            if patternNode.type == Kernel.Bind.type {
                return [patternNode.id: .Node(valueNode)]
            }

            guard patternNode.type == valueNode.type else {
                return nil
            }

            switch (patternNode.content, valueNode.content) {
            case (.Attrs(let patternAttrs), .Attrs(let valueAttrs)):
                // TODO: accumulate with a functional loop and banish `var`
                var childBindings: [NodeId: Node.Value] = [:]

                // Note: extra attributes in the value are ignored.
                for (attr, patternVal) in patternAttrs {
                    if let valueVal = valueAttrs[attr] {
                        switch (patternVal, valueVal) {

                        // This catches binding .Prim as well as .Node.
                        // TODO: normalize the same representation?
                        case (.Node(let pb), _) where pb.type == Kernel.Bind.type:
                            childBindings[pb.id] = valueVal

                        case (.Prim(let pp), .Prim(let vp)):
                            if vp == pp {
                                return [:]
                            }
                            else {
                                return nil
                            }
                        case (.Node(let pc), .Node(let vc)):
                            guard let newChildBindings = loop(patternNode: pc, valueNode: vc) else {
                                return nil
                            }

                            childBindings.merge(newChildBindings,
                                                uniquingKeysWith: { _,_ in fatalError("Duplicate binding ids") })
                        default:
                            return nil
                        }
                    }
                }

                return childBindings

            case (.Elems(let patternElems), .Elems(let valueElems)):
                fatalError("TODO")

            case (.Ref(_), .Ref(_)):
                // TODO: Hmm... 🤯
                fatalError("TODO")

            case (.Empty, .Empty):
                return [:]

            default:
                return nil
            }
        }

        if let bound = loop(patternNode: pattern, valueNode: rtNode),
           bindings.allSatisfy({ id in bound.keys.contains(id) }) {
//            print("matched: \(rtNode)")
//            print("bound: \(bound)")
            return .Matched(bindings.map { id in .Val(bound[id]!) })
        }
        else {
//            print("no match: \(rtNode)")
            return .NoMatch
        }

//        // HACK: this will work for simple literal values and even exact node matches:
//        if Diff.changes(from: pattern, to: rtNode) == [] {
//            return .Matched([])
//        }
//        else {
//            return .NoMatch
//        }
    }


// MARK: - Reverse translation, aka "repr", aka "print"

    /// Convert the result of evaluation back to a value in the kernel. Wherever possible, the result is
    /// an expression that evaluates to the same value.
    /// Ordinary values are simply returned, but unevaluated functions and errors both get converted
    /// back to "expressions" that aren't very useful when evaluated.
    static func repr(_ value: Eval.Value<Node.Value>) -> Node {
        switch value {
        case .Val(.Node(let node)):
            return node

        case .Val(.Prim(let prim)):
            switch prim {
            case .Nil:
                return Node(freshNodeId(),
                            Nil.type,
                            .Empty)

            case .Bool(_):
                return Node(freshNodeId(),
                            Bool_.type,
                            .Attrs([Bool_.value: .Prim(prim)]))

            case .Int(_):
                return Node(freshNodeId(),
                            Int_.type,
                            .Attrs([Int_.value: .Prim(prim)]))

            case .String(_):
                return Node(freshNodeId(),
                            String_.type,
                            .Attrs([String_.value: .Prim(prim)]))
            }

        case .Fn(let arity, _):
            return Node(freshNodeId(),
                        Fn.type,
                        .Attrs([
                            Fn.arity: .Prim(.Int(arity))
                        ]))

        case .Error(let err):
            return Node(freshNodeId(),
                        Error.type,
                        .Attrs([
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

        /// Note: this name is optional and not used during evaluation, but should probably be provided whenever code
        /// could possible be displayed to a human.
        public static let name = AttrName(type, "name")
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
