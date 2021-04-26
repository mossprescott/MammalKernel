/// Temporary!
///
/// Utility for constructing Kernel language programs in code. This is useful for tests. Once a proper editor exists, there will be less need
/// for this sort of thing.
public class KernelGen {
    let idGen = IdGen.Shared

    public init() {}

    public func Nil() -> Node {
        return Node(idGen.generateId(),
                    Kernel.Nil.type,
                    .Empty)
    }

    public func Bool(_ val: Bool) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Bool_.type,
                    .Attrs([
                        Kernel.Bool_.value: .Prim(.Bool(val))
                     ]))
    }

    public func Int(_ val: Int) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Int_.type,
                    .Attrs([
                        Kernel.Int_.value: .Prim(.Int(val))
                     ]))
    }

    public func String(_ val: String) -> Node {
        return Node(idGen.generateId(),
                    Kernel.String_.type,
                    .Attrs([
                        Kernel.String_.value: .Prim(.String(val))
                     ]))
    }

    /// A function that, when called, generates a new Var referring to a certain Bind
    public typealias VarGen = () -> Node

    /// Make a Bind node and a generator for Vars that refer to it. Note: `Let` and `Lambda` take care of this for you, but the
    /// individual parts might be needed for constructing patterns.
    public func bindGen(name: String? = nil) -> (Node, VarGen) {
        let bind = Node(idGen.generateId(),
                        Kernel.Bind.type,
                        name.map { .Attrs([Kernel.Bind.name: .Prim(.String($0))]) } ?? .Empty)
        return (bind,
                {
                    Node(self.idGen.generateId(),
                         Kernel.Var.type,
                         .Ref(bind.id))
                })
    }

    public func Let(expr: Node, name: String? = nil, body: (VarGen) -> Node) -> Node {
        let (bind, ref) = bindGen(name: name)
        return Node(idGen.generateId(),
                    Kernel.Let.type,
                    .Attrs([
                        Kernel.Let.bind: .Node(bind),
                        Kernel.Let.expr: .Node(expr),
                        Kernel.Let.body: .Node(body(ref))
                    ]))
    }

    public func Lambda(arity: Int, body: (VarGen, [VarGen]) -> Node) -> Node {
        return Lambda(paramNames: Array(repeating: nil, count: arity), body: body)
    }

    public func Lambda(paramNames: [String?], body: (VarGen, [VarGen]) -> Node) -> Node {
        let lambdaId = idGen.generateId()
        let params = paramNames.map(bindGen)

        return Node(lambdaId,
                    Kernel.Lambda.type,
                    .Attrs([
                        Kernel.Lambda.params:
                            .Node(Node(idGen.generateId(),
                                       Kernel.Params.type,
                                       .Elems(params.map { $0.0 }))),
                        Kernel.Lambda.body:
                            .Node(body({ () in
                                        Node(idGen.generateId(),
                                             Kernel.Var.type,
                                             .Ref(lambdaId)) },
                                       params.map { $0.1 }))
                    ]))
    }

    public func App(fn: Node, args: [Node]) -> Node {
        return Node(idGen.generateId(),
                    Kernel.App.type,
                    .Attrs([
                        Kernel.App.fn: .Node(fn),
                        Kernel.App.args: .Node(Node(idGen.generateId(),
                                                    Kernel.Args.type,
                                                    .Elems(args))),
                    ]))
    }

    public func Quote(body: Node) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Quote.type,
                    .Attrs([
                        Kernel.Quote.body: .Node(body),
                            ]))
    }

    public func Unquote(expr: Node) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Unquote.type,
                    .Attrs([
                        Kernel.Unquote.expr: .Node(expr),
                            ]))
    }

    public func UnquoteSplice(expr: Node) -> Node {
        return Node(idGen.generateId(),
                    Kernel.UnquoteSplice.type,
                    .Attrs([
                        Kernel.UnquoteSplice.expr: .Node(expr),
                            ]))
    }

    public func Match(expr: Node, pattern: Node, body: Node, otherwise: Node) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Match.type,
                    .Attrs([
                        Kernel.Match.expr: .Node(expr),
                        Kernel.Match.pattern: .Node(pattern),
                        Kernel.Match.body: .Node(body),
                        Kernel.Match.otherwise: .Node(otherwise),
                    ]))
    }

    public func Fn(arity: Int) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Fn.type,
                    .Attrs([
                        Kernel.Fn.arity: .Prim(.Int(arity))
                    ]))
    }

    public func Error(description: String) -> Node {
        return Node(idGen.generateId(),
                    Kernel.Error.type,
                    .Attrs([
                        Kernel.Error.description: .Prim(.String(description))
                    ]))
    }

    /// An empty node with the given type, which will usually refer to a "constant" (i.e. a builtin).
    public func Constant(_ type: NodeType) -> Node {
        return Node(idGen.generateId(), type, .Empty)
    }
}
