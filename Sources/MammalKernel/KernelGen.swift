/// Temporary!
///
/// Utility for constructing Kernel language programs in code. This is useful for tests. Once a proper editor exists, there will be less need
/// for this sort of thing.
public class KernelGen {
    var idGen = IdGen()

    public init() {}

    public func Nil() -> Node {
        return Node(id: idGen.generateId(),
                    type: Kernel.Nil.type,
                    content: .Empty)
    }

    public func Bool(_ val: Bool) -> Node {
        return Node(id: idGen.generateId(),
                     type: Kernel.Bool_.type,
                     content: .Attrs([
                        Kernel.Bool_.value: .Prim(.Bool(val))
                     ]))
    }

    public func Int(_ val: Int) -> Node {
        return Node(id: idGen.generateId(),
                     type: Kernel.Int_.type,
                     content: .Attrs([
                        Kernel.Int_.value: .Prim(.Int(val))
                     ]))
    }

    public func String(_ val: String) -> Node {
        return Node(id: idGen.generateId(),
                     type: Kernel.String_.type,
                     content: .Attrs([
                        Kernel.String_.value: .Prim(.String(val))
                     ]))
    }

    public func Let(expr: Node, body: (() -> Node) -> Node) -> Node {
        let bind = Node(id: idGen.generateId(),
                        type: Kernel.Bind.type,
                        content: .Empty)
        func ref() -> Node {
            Node(id: idGen.generateId(),
                 type: Kernel.Var.type,
                 content: .Ref(bind.id))
        }
        return Node(id: idGen.generateId(),
                    type: Kernel.Let.type,
                    content: .Attrs([
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
