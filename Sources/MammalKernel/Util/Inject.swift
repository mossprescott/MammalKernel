//struct AttrProjection<T> {
//    var requiredAttrs: [AttrName: ]
//}


//func fromNat(node: Node) -> Int {
//    switch node.type {
//    case NodeType("nat", "zero"):
//        return 0
//    case NodeType("nat", "succ"):
//        requiredAttr(node, AttrName("nat", "pred"), expected: "<nat>") { pred in
//            pred + 1
//        }
//    default:
//        fatalError("!")
//    }
//}


/// Just defining a simple enum because `Result.Failure` is required to sub-type `Error`.
public enum Either<Left, Right> {
    case left(Left)
    case right(Right)

    func merge() -> Left where Left == Right {
        switch self {
        case .left(let val): return val
        case .right(let val): return val
        }
    }

    func mapLeft<L>(transform: (Left) throws -> L) rethrows -> Either<L, Right> {
        switch self {
        case .left(let val):
            return .left(try transform(val))
        case .right(let val):
            return .right(val)
        }
    }
}

/// Extract an attribute and attempt to translate it to some value, returning either the result or a String describing what went wrong.
public func requiredAttr<T>(
    _ node: Node, _ attr: AttrName,
    expected: String,
    handle: (Node.Value) -> T?
) -> Either<String, T> {
    switch node.content {
    case .Attrs(let attrs):
        if let val = attrs[attr] {
            if let expr = handle(val) {
                return .right(expr)
            }
            else {
                return .left("Unexpected value for attribute \(attr) at node \(node.id); expected \(expected), found: \(val)")
            }
        }
        else {
            return .left("Missing required attribute \(attr) at node \(node.id)")
        }
    default:
        return .left("Missing required attribute \(attr) at node \(node.id) (no attributes present)")
    }
}

public func requiredAttrNode<T>(
    _ node: Node, _ attr: AttrName,
    expected: String,
    handle: (Node) -> T?
    ) -> Either<String, T> {
    requiredAttr(node, attr, expected: expected) { val in
        switch val {
        case .Prim(_):
            return nil
        case .Node(let node):
            return handle(node)
        }
    }
}

/// Extract an attribute and attempt to translate it to some value, or if the attribute is missing, substitute the provided default; returning
/// either the result or a String describing what went wrong.
/// Note: if the node isn't capable of having attributes (because it has `Elems` or a `Ref`), it's an error, but not if it's `Empty` (which
/// is considered isomorphic to `Attrs` with none present.)
public func optionalAttr<T>(
    _ node: Node, _ attr: AttrName,
    expected: String,
    defaultValue: T,
    handle: (Node.Value) -> T?
) -> Either<String, T> {
    switch node.content {
    case .Attrs(let attrs):
        if let val = attrs[attr] {
            if let expr = handle(val) {
                return .right(expr)
            }
            else {
                return .left("Unexpected value for attribute \(attr) at node \(node.id); expected \(expected), found: \(val)")
            }
        }
        else {
            return .right(defaultValue)
        }
    case .Empty:
        return .right(defaultValue)
    default:
        return .left("Missing required attribute \(attr) at node \(node.id) (no attributes present)")
    }
}

public func optionalAttrNode<T>(
    _ node: Node, _ attr: AttrName,
    expected: String,
    defaultValue: T,
    handle: (Node) -> T?
) -> Either<String, T> {
    optionalAttr(node, attr, expected: expected, defaultValue: defaultValue) { val in
        switch val {
        case .Prim(_):
            return nil
        case .Node(let node):
            return handle(node)
        }
    }
}
