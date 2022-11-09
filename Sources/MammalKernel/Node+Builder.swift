///// New `Node` with a unique, freshly generated id, and `Attrs` content.
///// Note: this isn't a Node constructor because it's not pure (you get a different id each time), which feels wrong for struct.
//public func makeNode(_ type: NodeType, attrs: [AttrName: Node.Value]) -> Node {
//    Node(IdGen.Shared.generateId(), type, .Attrs(attrs))
//}
//
///// New `Node` with a unique, freshly generated id, and `Elems` content.
///// Note: this isn't a Node constructor because it's not pure (you get a different id each time), which feels wrong for struct.
//public func makeNode(_ type: NodeType, elems: [Node]) -> Node {
//    Node(IdGen.Shared.generateId(), type, .Elems(elems))
//}
//
///// New Node with a unique, freshly generated id, and `Ref` content.
///// Note: this isn't a Node constructor because it's not pure (you get a different id each time), which feels wrong for struct.
//public func makeNode(_ type: NodeType, refTarget: NodeId) -> Node {
//    Node(IdGen.Shared.generateId(), type, .Ref(refTarget))
//}
//
///// New Node with a unique, freshly generated id, and `Empty` content.
///// Note: this isn't a Node constructor because it's not pure (you get a different id each time), which feels wrong for struct.
//public func makeEmptyNode(_ type: NodeType) -> Node {
//    Node(IdGen.Shared.generateId(), type, .Empty)
//}

/// DSL for building nodes in code.
///
/// ```
/// let program = Elems(NodeType("example", "declarations")) {
///
/// }
/// .build()
/// ```
///
/// A nodebuilder is a pure value which describes the shape of a tree; when `build` is called, it will generate an instance of the tree
/// with a newly-generated id for each node.
public protocol NodeBuilder {
    func build(_ idGen: @escaping () -> NodeId) -> Node
}

public typealias Builder<T> = (@escaping () -> NodeId) -> T

/// *Warning*: if the same attribute name appears more than once, the first occurence is used.
public struct Attrs: NodeBuilder {
    var type: NodeType
    var buildAttrs: Builder<[AttrName: Node.Value]>

    public init(_ type: NodeType, @AttrsBuilder buildAttrs: @escaping () -> Builder<[AttrName: Node.Value]>) {
        self.type = type
        self.buildAttrs = buildAttrs()
    }

    public func build(_ idGen: @escaping () -> NodeId = IdGen.Shared.generateId) -> Node {
        Node(idGen(), type, .Attrs(buildAttrs(idGen)))
    }
}

@resultBuilder public struct AttrsBuilder {
    /// Wrap `nil` as the value of an attribute. Ok, this type is perverse, but it permits only `nil`, which makes it explicit.
    public static func buildExpression(_ pair: (AttrName, Never?)) -> Builder<[AttrName: Node.Value]> {
        { _ in [pair.0: .Prim(.Nil)] }
    }

    public static func buildExpression(_ pair: (AttrName, Bool)) -> Builder<[AttrName: Node.Value]> {
        { _ in [pair.0: .Prim(.Bool(pair.1))] }
    }

    public static func buildExpression(_ pair: (AttrName, Int)) -> Builder<[AttrName: Node.Value]> {
        { _ in [pair.0: .Prim(.Int(pair.1))] }
    }

    public static func buildExpression(_ pair: (AttrName, String)) -> Builder<[AttrName: Node.Value]> {
        { _ in [pair.0: .Prim(.String(pair.1))] }
    }

    public static func buildExpression(_ pair: (AttrName, NodeBuilder)) -> Builder<[AttrName: Node.Value]> {
        { idGen in [pair.0: .Node(pair.1.build(idGen))] }
    }

    /// For the case that the type of value isn't known until runtime.
    public static func buildExpression(_ pair: (AttrName, Node.Value)) -> Builder<[AttrName: Node.Value]> {
        { _ in [pair.0: pair.1] }
    }

    public static func buildBlock(_ elems: Builder<[AttrName: Node.Value]>...) -> Builder<[AttrName: Node.Value]> {
        { idGen in elems.reduce([:]) { acc, b in
            var acc = acc
            acc.merge(b(idGen), uniquingKeysWith: { first, _ in first })
            return acc
            }
        }
    }

    public static func buildOptional(_ elem: Builder<[AttrName: Node.Value]>?) -> Builder<[AttrName: Node.Value]> {
        elem ?? { _ in [:] }
    }
}

public struct Elems: NodeBuilder {
    var type: NodeType
    var buildElems: Builder<[Node]>

    public init(_ type: NodeType, @ElemsBuilder buildElems: @escaping () -> Builder<[Node]>) {
        self.type = type
        self.buildElems = buildElems()
    }

    public func build(_ idGen: @escaping () -> NodeId = IdGen.Shared.generateId) -> Node {
        Node(idGen(), type, .Elems(buildElems(idGen)))
    }
}

@resultBuilder public struct ElemsBuilder {
    public static func buildExpression(_ node: NodeBuilder) -> Builder<[Node]> {
        { idGen in [node.build(idGen)] }
    }

    public static func buildBlock(_ elems: Builder<[Node]>...) -> Builder<[Node]> {
        { idGen in elems.flatMap { $0(idGen) } }
    }

    public static func buildOptional(_ elem: Builder<[Node]>?) -> Builder<[Node]> {
        elem ?? { _ in [] }
    }
}

public struct Empty: NodeBuilder {
    var type: NodeType

    public init(_ type: NodeType) {
        self.type = type
    }

    public func build(_ idGen: () -> NodeId) -> Node {
        Node(idGen(), type, .Empty)
    }
}

/// Introduce a scope containing an arbitrary target node and some Ref nodes that refer to it.
public struct Scope: NodeBuilder {
    var target: NodeBuilder
    var content: (NodeBuilder, @escaping (NodeType) -> NodeBuilder) -> NodeBuilder

    public init(_ target: NodeBuilder, content: @escaping (NodeBuilder, @escaping (NodeType) -> NodeBuilder) -> NodeBuilder) {
        self.target = target
        self.content = content
    }

    public func build(_ idGen: @escaping () -> NodeId) -> Node {
        let tb = TargetBuilder(target: target, idGen: idGen)
        let cb = { type in RefBuilder(type: type, targetId: tb.targetId) }
        return content(tb, cb).build(idGen)
    }

    struct RefBuilder: NodeBuilder {
        let type: NodeType
        let targetId: NodeId

        public func build(_ idGen: @escaping () -> NodeId) -> Node {
            Node(idGen(), type, .Ref(targetId))
        }
    }

    class TargetBuilder: NodeBuilder {
        let target: NodeBuilder
        let idGen: () -> NodeId
        let targetId: NodeId
        var targetIdUsed: Bool

        init(target: NodeBuilder, idGen: @escaping () -> NodeId) {
            self.target = target
            self.idGen = idGen
            self.targetId = idGen()
            self.targetIdUsed = false
        }

        func build(_ idGen: @escaping () -> NodeId) -> Node {
            target.build { [self] in
                if targetIdUsed {
                    return idGen()
                }
                else {
                    targetIdUsed = true
                    return targetId
                }
            }
        }
    }
}

/// Introduce a scope containing an arbitrary target node and some Ref nodes that refer to it. 
///
/// In this alternative, the bound node is supplied at the point where it appears. If `bind` is never used, any refs are effectively dangling.
/// If `bind` is used more than once, the first result is the target of all references (so, don't do that.)
public struct LazyScope: NodeBuilder {
    public struct Refs {
        var bind: (NodeBuilder) -> NodeBuilder

        var ref: (NodeType) -> NodeBuilder
    }

    var content: (Refs) -> NodeBuilder

    public init(content: @escaping (Refs) -> NodeBuilder) {
        self.content = content
    }

    public func build(_ idGen: @escaping () -> NodeId) -> Node {
        let tb = TargetBuilder(idGen)
        let refs = Refs(
            bind: { target in
                tb.target = target
                return tb
            },
            ref: { type in RefBuilder(type: type, targetId: tb.targetId) }
        )
        return content(refs).build(idGen)
    }

    struct RefBuilder: NodeBuilder {
        let type: NodeType
        let targetId: NodeId

        public func build(_ idGen: @escaping () -> NodeId) -> Node {
            Node(idGen(), type, .Ref(targetId))
        }
    }

    class TargetBuilder: NodeBuilder {
        var target: NodeBuilder?
        let idGen: () -> NodeId
        let targetId: NodeId
        var targetIdUsed: Bool

        init(_ idGen: @escaping () -> NodeId) {
            self.target = nil
            self.idGen = idGen
            self.targetId = idGen()
            self.targetIdUsed = false
        }

        func build(_ idGen: @escaping () -> NodeId) -> Node {
            guard let target = target else {
                // This can't happen because the builder is only ever added to the construction when
                // the target is supplied.
                fatalError("The loop is unclosed?!")
            }

            if targetIdUsed {
                print("Warning: redundant/conflicting binds!")
            }

            return target.build { [self] in
                if targetIdUsed {
                    return idGen()
                }
                else {
                    targetIdUsed = true
                    return targetId
                }
            }
        }
    }
}


public func builderExample1(hasBaz: Bool) -> Node {
    Elems(NodeType("example", "foo")) {
        Empty(NodeType("example", "bar"))

        Elems(NodeType("example", "nested")) {
            Empty(NodeType("example", "bar"))
        }

        if hasBaz {
            Empty(NodeType("example", "baz") )
        }

        Attrs(NodeType("example", "quux")) {
            (AttrName("example", "nil"), nil)
            (AttrName("example", "maybe"), false)
            if !hasBaz {
                (AttrName("example", "num"), 1)
                (AttrName("example", "msg"), "hi!")
            }
            (AttrName("example", "child"),
             Empty(NodeType("example", "bar")))
        }

        // Ok, well, this does seem to work, but it's fairly obtuse.
        Scope(Empty(NodeType("example", "bind"))) { bind, ref in
            let declType = NodeType("example", "declaration")
            return Attrs(declType) {
                (AttrName(declType, "bind"),
                 bind)

                (AttrName(declType, "expr"),
                 Elems(NodeType("example", "exprs")) {
                    Empty(NodeType("example", "something"))
                    ref(NodeType("example", "ref"))
                 })
            }
        }

        // This is perhaps slightly less obtuse, but it's unsafe.
        LazyScope { scope in
            let declType = NodeType("example", "declaration")
            return Attrs(declType) {
                (AttrName(declType, "bind"),
                 scope.bind(Empty(NodeType("example", "bind"))))

                // A second attempt to bind ends up just being unreferenced:
                (AttrName(declType, "bind2"),
                 scope.bind(Empty(NodeType("example", "bind"))))

                (AttrName(declType, "expr"),
                 Elems(NodeType("example", "exprs")) {
                    Empty(NodeType("example", "something"))
                    scope.ref(NodeType("example", "ref"))
                 })
            }
        }

        LazyScope { scope in
            Elems(NodeType("example", "exprs")) {
                scope.ref(NodeType("example", "dangling"))
            }
        }
    }
    .build()
}

///// *Experimental*: result-builder-style DSL for constructing Nodes
//public struct Elems {
//    public init(_ type: NodeType, @ElemsBuilder content: () -> Content) {
//        self.id = IdGen.Shared.generateId()
//        self.type = type
//        self.content = content()
//    }
//}
//@resultBuilder public struct ElemsBuilder {
////        ///
////        public static func buildBlock() -> Content {
////            .Empty
////        }
//
//    public static func buildBlock(_ content: Node...) -> Content {
//        .Elems(content)
//    }
//}
//
//public struct Attrs {
//    public init(_ type: NodeType, @AttrsBuilder content: () -> Content) {
//        self.id = IdGen.Shared.generateId()
//        self.type = type
//        self.content = content()
//    }
//
//    @resultBuilder public struct AttrsBuilder {
////        ///
////        public static func buildBlock() -> Content {
////            .Empty
////        }
//
//        public static func buildBlock(_ content: Attr...) -> Content {
//            .Attrs(Dictionary(uniqueKeysWithValues: content.map { ($0.name, $0.value) }))
//        }
//    }
//
//    /// Terrible name? This is just for the DSL. Overloads for each kind of value.
//    public struct Attr {
//        var name: AttrName
//        var value: Node.Value
//
//        /// Mainly useful for constructing `nil` values, which more or less never happens.
//        public init(_ name: AttrName, primitive value: Primitive) {
//            self.name = name
//            self.value = .Prim(value)
//        }
//
//        public init(_ name: AttrName, bool value: Bool) {
//            self.name = name
//            self.value = .Prim(.Bool(value))
//        }
//
//        public init(_ name: AttrName, int value: Int) {
//            self.name = name
//            self.value = .Prim(.Int(value))
//        }
//
//        public init(_ name: AttrName, string value: String) {
//            self.name = name
//            self.value = .Prim(.String(value))
//        }
//
//        public init(_ name: AttrName, node: Node) {
//            self.name = name
//            self.value = .Node(node)
//        }
//    }
//}
//
//public let dumbTest: Node = {
//    let fooType = NodeType("test", "foo")
//    let barAttr = AttrName(fooType, "bar")
//    let bazAttr = AttrName(fooType, "baz")
//    let barType = NodeType("test", "bar")
//
//    let node0 =
//        Node(fooType) {
//        }
//    print(node0)
//
//    let node1 =
//        Node(fooType) {
//            Node(barType) {}
//        }
//    print(node1)
//
//    let node2 =
//        Node(fooType) {
//            Node(barType) {}
//            Node(barType) {}
//        }
//    print(node2)
//
//    let node3 =
//        Node(fooType) {
//            Node.Attr(barAttr, bool: false)
//            Node.Attr(bazAttr, node:
//             Node(barType) {})
//        }
//    print(node3)
//
//
//    return node1
//}()
