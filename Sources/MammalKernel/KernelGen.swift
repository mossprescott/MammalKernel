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

    public func Let(expr: Node, body: (() -> Node) -> Node) -> Node {
        let bind = Node(idGen.generateId(),
                        Kernel.Bind.type,
                        .Empty)
        func ref() -> Node {
            Node(idGen.generateId(),
                 Kernel.Var.type,
                 .Ref(bind.id))
        }
        return Node(idGen.generateId(),
                    Kernel.Let.type,
                    .Attrs([
                        Kernel.Let.bind: .Node(bind),
                        Kernel.Let.expr: .Node(expr),
                        Kernel.Let.body: .Node(body(ref))
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
