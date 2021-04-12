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

/// Every node has an `id`,  `type`, and some kind of content, which may be a collection of attributes,
/// a sequence of nodes, a reference to another node, or nothing at all.
public struct Node {
    var id: NodeId
    var type: NodeType
    var content: Content

    public enum Content {
        /// An un-ordered collection of named attributes, each of which is a value/Node.
        case Attrs([AttrName: Value])

        /// An ordered list of children, each a Node.
        case Elems([Node])

        /// A reference to another Node.
        case Ref(NodeId)

        /// No contents; the node's meaning is carried entirely by its type. Note: this is here mostly
        /// so you don't have to ask yourself whether an empty `Attrs` or `Elems` is appropriate.
        case Empty
    }
}

/// A value may be a Primitive (leaf) or Node.
public enum Value {
    case Prim(Primitive)
    case Node(Node)
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

/// Check structural constraints:
///
/// - every node has a unique id
/// - every Ref refers to a node that is present (no free vars), and is not the Ref itself
/// - TODO: what else?
public func checkWellFormedness(value: Value) -> Bool {
    fatalError("TODO")
}
