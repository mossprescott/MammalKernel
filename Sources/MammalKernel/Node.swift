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
    var language: String
    var name: String

    public var fullName: String {
        language + "/" + name
    }

    public var simpleName: String {
        name
    }

    public init(_ language: String, _ name: String) {
        self.language = language
        self.name = name
    }
}

/// "Symbol" that indicates the meaning of an attribute with respect the to node that contains it.
///
/// To avoid unintentional collisions, each name consists of a "language" prefix, (optional) type name,
/// and attribute name separated by a slash.
/// For example, a "lambda" expression in the kernel language will have an attribute `"kernel/lambda/body"`
/// which contains the expression which is the body of the function.
public struct AttrName: Hashable {
    var prefix: String
    var name: String

    public var fullName: String {
        prefix + "/" + name
    }

    public var simpleName: String {
        name
    }

    /// Compose an attribute name from a node type plus a unique short name
    public init(_ parent: NodeType, _ name: String) {
        self.prefix = parent.fullName
        self.name = name
    }

    /// For attributes that are used with more than one type of node.
    public init(_ language: String, _ name: String) {
        self.prefix = language
        self.name = name
    }
}

/// Every node has an `id`,  `type`, and some kind of content, which may be a collection of attributes,
/// a sequence of nodes, a reference to another node, or nothing at all.
public struct Node: CustomDebugStringConvertible {
    public var id: NodeId
    public var type: NodeType
    public var content: Content

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

    /// The value stored as an attribute may be a primitive or node. That's the only way a primitive can
    /// appear in a tree.
    public enum Value {
        case Prim(Primitive)
        case Node(Node)
    }

    /// Simplified constructor; no need to name each field, since the types are distinct and the meaning of each is clear.
    public init(_ id: NodeId, _ type: NodeType, _ content: Content) {
        self.id = id
        self.type = type
        self.content = content
    }

    /// Namespace hiding away some handy functions that shouldn't be used lightly.
    public enum Util {
        /// Construct a dictionary of every node in the tree (including the root), keyed by id.
        /// *WARNING*: this isn't cheap and should be avoided when possible. 
        public static func descendantsById(of root: Node) -> [NodeId: Node] {
            var nodesById: [NodeId: Node] = [:]
            func loop(_ node: Node) {
                nodesById[node.id] = node
                switch node.content {
                case .Attrs(let attrs):
                    for val in attrs.values {
                        switch val {
                        case .Node(let child): loop(child)
                        default: break
                        }
                    }
                case .Elems(let elems):
                    for child in elems { loop(child) }
                case .Ref(_), .Empty:
                    break
                }
            }
            loop(root)
            return nodesById
        }

        // List of direct child nodes, if any, in no particular order.
        public static func children(of node: Node) -> [Node] {
            switch node.content {
            case .Attrs(let attrs):
                return attrs.values.compactMap { val in
                    switch val {
                    case .Prim(_): return nil
                    case .Node(let node): return node
                    }
                }
            case .Elems(let elems):
                return elems
            case .Ref(_), .Empty:
                return []
            }
        }

        /// List of ancestors forming a path to the root of the tree, if it is present. The first element of the result is the target node, and
        /// the last is the given root. If the target appears more than once, a single path is chosen at random.
        /// *WARNING*: this isn't cheap and should be avoided when possible.
        public static func ancestors(of targetId: NodeId, within root: Node) -> [Node]? {
            func loop(_ node: Node) -> [Node]? {
                if node.id == targetId {
                    return [node]
                }
                else {
                    for child in children(of: node) {
                        if let partialResult = loop(child) {
                            return partialResult + [node]
                        }
                    }
                    return nil
                }
            }
            return loop(root)
        }
    }

    /// A reasonably human-readable, fairly compressed, nicely indented, mostly unambiguous, string representation.
    public var debugDescription: String {
        func writeNode(_ node: Node) -> [String] {
            func writeContent(_ content: Node.Content) -> [String] {
                switch content {
                case .Attrs(let attrs):
                    return attrs.map { attr, v -> [String] in
                        let name = attr.name.remove(prefix: node.type.fullName + "/")
                        switch v {
                        case .Prim(.Nil):
                            return ["- \(name): nil"]
                        case .Prim(.Bool(let x)):
                            return ["- \(name): \(x)"]
                        case .Prim(.Int(let x)):
                            return ["- \(name): \(x)"]
                        case .Prim(.String(let x)):
                            return ["- \(name): \(x.debugDescription)"]
                        case .Node(let child):
                            return ["- \(name):"]
                                + writeNode(child).map { "    " + $0 }
                        }
                    }.flatMap { $0 }

                case .Elems(let elems):
                    return elems.flatMap(writeNode)

                case .Ref(let target):
                    return ["ref: \(target.id)"]

                case .Empty:
                    return []
                }
            }
            return ["\(node.type.fullName) [\(node.id.id)]"] +
                writeContent(node.content).map { "  " + $0 }
        }
        return writeNode(self).joined(separator: "\n")
    }
}

extension String {
    func remove(prefix: String) -> Substring {
        if self.starts(with: prefix) {
            return self.dropFirst(prefix.count)
        }
        else {
            return Substring(self)
        }
    }
}

/// Primitive values which may appear as attributes. Note: more primitives will be added as
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
public func checkWellFormedness(value: Node) -> Bool {
    fatalError("TODO")
}
