/// Node and attribute types defining the Mammal Diff language, and code to generate them by inspecting
/// "old" and "new" nodes.
///
/// Diffs are useful for tracking how trees change over time (for example, in a version control system), and for comparing
/// actual results with expectations (for example, in unit tests.)
///
/// TODO: make it a proper language, and provide variants to get just the changes, or to get a document with the
/// changes embedded within the common structure.
public enum Diff {
    /// Compare two trees, ignoring ids but reporting all other differences.
    /// The value on the right is considered the "new" value: if it contains somethng thats not
    /// in the left value, the new thing is "added"; if the reverse, the thing is "missing"
    public static func changes(from old: Node, to new: Node) -> [NodeDelta] {
        if old.type != new.type {
            return [.nodeTypesDiffer(old.type, new.type)]
        }
        else {
            switch (old.content, new.content) {
            case (.Attrs(let lAttrs), .Attrs(let rAttrs)):
                let lNames = Set(lAttrs.keys)
                let rNames = Set(rAttrs.keys)
                let missing = Array(lNames.subtracting(rNames))
                    .map { NodeDelta.attributeMissing($0) }
                let added = Array(rNames.subtracting(lNames))
                    .map { NodeDelta.attributeAdded($0) }
                let children = Array(lNames.union(rNames)).flatMap { name -> [NodeDelta] in
                    switch (lAttrs[name]!, rAttrs[name]!) {
                    case (.Prim(let lPrim), .Prim(let rPrim)):
                        if lPrim == rPrim {
                            return []
                        }
                        else {
                            return [.primitivesDiffer(lPrim, rPrim)]
                        }
                    case (.Node(let lNode), .Node(let rNode)):
                        return changes(from: lNode, to: rNode)
                    case (.Prim(let lPrim), .Node(let rNode)):
                        return [.nodeReplacedPrimitive(lPrim, rNode.type)]
                    case (.Node(let lNode), .Prim(let rPrim)):
                        return [.primitiveReplacedNode(lNode.type, rPrim)]
                    }
                }
                return missing + added + children

            case (.Elems(let lElems), .Elems(let rElems)):
                // TODO: detect insertions and deletions, when some matching elements are present
                let missing = Array(repeating: NodeDelta.elementMissing,
                                    count: lElems.count - rElems.count)
                let added = Array(repeating: NodeDelta.elementAdded,
                                  count: rElems.count - lElems.count)
                let children = zip(lElems, rElems).flatMap { (l, r) in
                    changes(from: l, to: r)
                }
                return missing + added + children

            case (.Ref(let lTarget), .Ref(let rTarget)):
                // Look up the referenced node in each tree. They need to be structurally equivalent,
                // and maybe also appear in the same location in the tree.
                fatalError("TODO")

            case (.Empty, .Empty):
                return []

            case (_, _):
                func contentType(_ content: Node.Content) -> String {
                    switch content {
                    case .Attrs(_): return "Attrs"
                    case .Elems(_): return "Elems"
                    case .Ref(_): return "Ref"
                    case .Empty: return "Empty"
                    }
                }
                return [.contentTypesDiffer(contentType(old.content), contentType(new.content))]
            }
        }
    }

    /// TODO: include NodeId or path or something as a location for each diff
    public enum NodeDelta: Equatable {
        case nodeTypesDiffer(NodeType, NodeType)
        case primitivesDiffer(Primitive, Primitive)
        case nodeReplacedPrimitive(Primitive, NodeType)
        case primitiveReplacedNode(NodeType, Primitive)
        case attributeMissing(AttrName)
        case attributeAdded(AttrName)
        case elementMissing  // TODO: include what info?
        case elementAdded  // TODO: include what info?
        case contentTypesDiffer(String, String)  // TODO: what info, actually?
    }
}
