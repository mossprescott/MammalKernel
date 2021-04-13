/// Temporary!
///
/// Utility for constructing Kernel language programs in code. This is useful for tests. Once a proper editor exists, there will be less need
/// for this sort of thing.
public class KernelGen {
    var idGen = IdGen()

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

    /// Make a Bind node and a generator for Vars that refer to it.
    func bindGen() -> (Node, VarGen) {
        let bind = Node(idGen.generateId(),
                        Kernel.Bind.type,
                        .Empty)
        return (bind,
                {
                    Node(self.idGen.generateId(),
                         Kernel.Var.type,
                         .Ref(bind.id))
                })
    }

    public func Let(expr: Node, body: (VarGen) -> Node) -> Node {
        let (bind, ref) = bindGen()
        return Node(idGen.generateId(),
                    Kernel.Let.type,
                    .Attrs([
                        Kernel.Let.bind: .Node(bind),
                        Kernel.Let.expr: .Node(expr),
                        Kernel.Let.body: .Node(body(ref))
                    ]))
    }

    public func Lambda(arity: Int, body: (VarGen, [VarGen]) -> Node) -> Node {
        let lambdaId = idGen.generateId()
        let params = Array(repeating: (), count: arity).map(bindGen)

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

    // TODO: more special forms
}

struct IdGen {
    var nextId = 0

    mutating func generateId() -> NodeId {
        let id = NodeId(nextId)
        nextId += 1
        return id
    }
}
