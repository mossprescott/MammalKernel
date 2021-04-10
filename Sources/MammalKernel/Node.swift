/*
 Common representation for all data (and programs) in Mammal.

 TODO: describe the general properties of these values and how they get assembled into
 programs, etc.

 For now, trying to keep these types as simple and clean as possible, but they are the
 basic currency of all computation (including at runtime), so might come back and make
 them uglier for the sake of efficiency later.
 */

/// Identifies a node. Unique with respect to all the other Nodes appearing in the same program/document/tree.
public struct NodeId: Hashable {
    var id: Int

    public init(_ id: Int) {
        self.id = id
    }
}

/// "Symbol" that indicates the meaning of the node. Every node which has the same meaning should have the
/// same type, but a program will typically contain many nodes of each type.
///
/// To avoid unintentional collisions, each type consists of a "language" prefix and type name
/// separated by a slash.
/// For example, an integer literal in the kernel language has the type `"kernel/int"`.
public struct NodeType: Hashable {
    var type: String

    public init(_ language: String, _ name: String) {
        self.type = language + "/" + name
    }
}

/// "Symbol" that indicates the meaning of an attribute with respect the to node that contains it.
///
/// To avoid unintentional collisions, each name consists of a "language" prefix, (optional) type name,
/// and attribute name separated by a slash.
/// For example, a "lambda" expression in the kernel language will have an attribute `"kernel/lambda/body"`
/// which contains the expression which is the body of the function.
public struct AttrName: Hashable {
    var name: String

    /// Compose an attribute name from a node type plus a unique short name
    public init(_ parent: NodeType, _ shortName: String) {
        self.name = parent.type + "/" + shortName
    }

    /// For attributes that are used with more than one type of node.
    public init(_ language: String, _ name: String) {
        self.name = language + "/" + name
    }

    /// For attributes that aren't always used with a specific node type.
    public init(fullName: String) {
        self.name = fullName
    }
}

/// A value may be a primitive (leaf) or Node. Every node has an `id` and `type`, primitives do not.
public enum Value {
    case Prim(Primitive)
    case Node(id: NodeId, type: NodeType, content: NodeContent)
}

/// Primitive values which may appear as attributes/elements. Note: more primitives will be added as
/// necessary, but the intention is to cover only the most commonly-used values, plus a very general
/// container for embedding arbitrary data.
public enum Primitive: Equatable {
    case Nil
    case Bool(Bool)
    case Int(Int)
    case String(String)
}

/// The content of a node may be a collection of attributes, a sequence of elements, a reference to
/// another node, or nothing at all.
public enum NodeContent {
    /// An un-ordered collection of named attributes, each of which is a value/Node.
    case Attrs([AttrName: Value])

    /// An ordered list of children, each a value/Node.
    case Elems([Value])

    /// A reference to another Node.
    case Ref(NodeId)

    /// No contents; the node's meaning is carried entirely by its type. Note: this is here mostly
    /// so you don't have to ask yourself whether an empty `Attrs` or `Elems` is appropriate.
    case Empty
}

/// Check structural constraints:
///
/// - every node has a unique id
/// - every Ref refers to a node that is present (no free vars), and is not the Ref itself
/// - TODO: what else?
public func checkWellFormedness(value: Value) -> Bool {
    fatalError("TODO")
}


// MARK: - Structural equality

/// TODO: include NodeId or path or something as a location for each diff
public enum ValueDelta: Equatable {
    case primitivesDiffer(Primitive, Primitive)
    case nodeTypesDiffer(NodeType, NodeType)
    case attributeMissing(AttrName)
    case attributeAdded(AttrName)
    case elementMissing  // TODO: include what info?
    case elementAdded  // TODO: include what info?
    case contentTypesDiffer(String, String)  // TODO: what info, actually?
}

extension Value {
    /// Compare two trees, ignoring ids but reporting all other differences.
    /// The value on the right is considered the "new" value: if it contains somethng thats not
    /// in the left value, the new thing is "added"; if the reverse, the thing is "missing"
    public func diff(_ newValue: Value) -> [ValueDelta] {
        switch (self, newValue) {
        case (.Prim(let l), .Prim(let r)):
            if l != r {
                return [.primitivesDiffer(l, r)]
            }
            else {
                return []
            }
        case (.Node(_, let lType, _), .Node(_, let rType, _))
                where lType != rType:
            return [.nodeTypesDiffer(lType, rType)]

        case (.Node(_, _, let lContent), .Node(_, _, let rContent)):
            switch (lContent, rContent) {

            case (.Attrs(let lAttrs), .Attrs(let rAttrs)):
                let lNames = Set(lAttrs.keys)
                let rNames = Set(rAttrs.keys)
                let missing = Array(lNames.subtracting(rNames))
                    .map { ValueDelta.attributeMissing($0) }
                let added = Array(rNames.subtracting(lNames))
                    .map { ValueDelta.attributeAdded($0) }
                let children = Array(lNames.union(rNames)).flatMap { name in
                    lAttrs[name]!.diff(rAttrs[name]!)
                }
                return missing + added + children

            case (.Elems(let lElems), .Elems(let rElems)):
                // TODO: detect insertions and deletions, when some matching elements are present
                let missing = Array(repeating: ValueDelta.elementMissing,
                                    count: lElems.count - rElems.count)
                let added = Array(repeating: ValueDelta.elementAdded,
                                  count: rElems.count - lElems.count)
                let children = zip(lElems, rElems).flatMap { (l, r) in
                    l.diff(r)
                }
                return missing + added + children

            case (.Ref(let lTarget), .Ref(let rTarget)):
                fatalError("TODO")

            case (.Empty, .Empty):
                return []

            case (_, _):
                func contentType(_ content: NodeContent) -> String {
                    switch content {
                    case .Attrs(_): return "Attrs"
                    case .Elems(_): return "Elems"
                    case .Ref(_): return "Ref"
                    case .Empty: return "Empty"
                    }
                }
                return [.contentTypesDiffer(contentType(lContent), contentType(rContent))]
            }

        case (_, _):
            fatalError("what am I missing?")
        }
    }
}
