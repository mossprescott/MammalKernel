import Foundation

/// Location of a single node within a tree of nodes, with operations to shift to any adjacent location, to modify the node at the location,
/// and to recover the original (or modified) tree.
///
/// See https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf, but note
/// that the mixed type of `Node` makes this all a bit more complicated than the classic functional zipper:
///
/// - An attribute may contain a node or a primitive value; only nodes participate in the tree of potential locations.
/// - For navigation purposes, it's useful to always have a notion of next-previous sibling, so the caller supplies
public struct Zipper {
    public var node: Node

    var path: Path

    /// An ordering for attributes which allows the set of attributes for any particular node to be treated as an ordered sequence.
    ///
    /// Should return true if the left named attribute comes "before" the right named attribute, when they both appear within a node of
    /// the given type, in whatever sense the caller chooses to define.
    ///
    /// Must be a _strict, weak ordering_ over the attribute names (see `Collection.sort(by:)`)
    public typealias AttrsInIncreasingOrder = (NodeType, AttrName, AttrName) -> Bool

    public var attrOrder: AttrsInIncreasingOrder

    /// An ordering of attributes which meets the requirements but doesn't satisfy user expectations.
    public static let arbitraryOrder: AttrsInIncreasingOrder = { _, x, y in
        x.fullName < y.fullName
    }

    public init(_ node: Node, attrOrder: @escaping AttrsInIncreasingOrder) {
        self.node = node
        self.path = .top
        self.attrOrder = attrOrder
    }

    init(_ node: Node, _ path: Path, attrOrder: @escaping AttrsInIncreasingOrder) {
        self.node = node
        self.path = path
        self.attrOrder = attrOrder
    }

    indirect enum Path {
        /// The location is at the root.
        case top

        /// The location is at a named attribute.
        case attr(NodeId, NodeType, AttrName, Path, [AttrName: Node.Value])

        /// The location is within a list of elements.
        case elem(NodeId, NodeType, [Node], Path, [Node])
    }


// MARK: - Navigation

    /// Move focus to the root of the tree.
    public func root() -> Zipper {
        if let parent = up() {
            return parent.root()
        }
        else {
            return self
        }
    }

    /// Move focus to the parent node, unless it's already at the root.
    public func up() -> Zipper? {
        switch path {
        case .top:
            return nil

        case .attr(let id, let type, let name, let parentPath, let other):
            var attrs = other
            attrs[name] = .Node(node)
            return Zipper(Node(id, type, .Attrs(attrs)), parentPath, attrOrder: attrOrder)

        case .elem(let id, let type, let left, let parentPath, let right):
            return Zipper(Node(id, type, .Elems(left.reversed() + [node] + right)), parentPath, attrOrder: attrOrder)
        }
    }

    /// Move focus to the previous sibling node, if any. If the location is within a set of named attributes, the supplied comparator is
    /// applied to determine the effective order.
    ///
    /// Note: this is usually called "left", but in the case of ASTs, siblings are laid out vertically as often as horizontally on screen.
    public func previous() -> Zipper? {
        switch path {
        case .top:
            return nil

        case .attr(let id, let type, let name, let parentPath, let other):
            let earlierPairs: [(AttrName, Node)] = other.compactMap { key, val in
                if attrOrder(type, key, name), case .Node(let node) = val {
                    return (key, node)
                }
                else {
                    return nil
                }
            }
            .sorted(by: { attrOrder(type, $1.0, $0.0) })

            if let (previousName, previousNode) = earlierPairs.first {
                var newOther = other
                newOther.removeValue(forKey: name)
                newOther[name] = .Node(node)
                return Zipper(previousNode, .attr(id, type, previousName, parentPath, newOther), attrOrder: attrOrder)
            }
            else {
                return nil
            }

        case .elem(let id, let type, let left, let parentPath, let right):
            if let previousNode = left.first {
                return Zipper(previousNode,
                              .elem(id, type, Array(left[1...]), parentPath, [node] + right),
                              attrOrder: attrOrder)
            }
            else {
                return nil
            }
        }
    }

    /// Move focus to the next sibling node, if any. If the location is within a set of named attributes, the supplied comparator is
    /// applied to determine the effective order.
    ///
    /// Note: this is usually called "right", but in the case of ASTs, siblings are laid out vertically as often as horizontally on screen.
    public func next() -> Zipper? {
        switch path {
        case .top:
            return nil

        case .attr(let id, let type, let name, let parentPath, let other):
            let laterPairs: [(AttrName, Node)] = other.compactMap { key, val in
                if attrOrder(type, name, key), case .Node(let node) = val {
                    return (key, node)
                }
                else {
                    return nil
                }
            }
            .sorted(by: { attrOrder(type, $0.0, $1.0) })

            if let (nextName, nextNode) = laterPairs.first {
                var newOther = other
                newOther.removeValue(forKey: name)
                newOther[name] = .Node(node)
                return Zipper(nextNode, .attr(id, type, nextName, parentPath, newOther), attrOrder: attrOrder)
            }
            else {
                return nil
            }

        case .elem(let id, let type, let left, let parentPath, let right):
            if let nextNode = right.first {
                return Zipper(nextNode,
                              .elem(id, type, [node] + left, parentPath, Array(right[1...])),
                              attrOrder: attrOrder)
            }
            else {
                return nil
            }
        }
    }

    /// Move focus to the first child node.
    public func down() -> Zipper? {
        switch node.content {
        case .Attrs(let attrs):
            let childPairs: [(AttrName, Node)] = orderedChildNodes(attrs)

            if let firstChild = childPairs.first {
                return Zipper(firstChild.1,
                              .attr(node.id, node.type, firstChild.0, path, attrs.filter { name, _ in name != firstChild.0 }),
                              attrOrder: attrOrder)
            }
            else {
                return nil
            }

        case .Elems(let elems):
            if let firstChild = elems.first {
                return Zipper(firstChild,
                              .elem(node.id, node.type, [], path, Array(elems[1...])),
                              attrOrder: attrOrder)
            }
            else {
                return nil
            }

        case .Ref, .Empty:
            return nil
        }
    }

    /// Move focus to a particular child node by attribute name.
    public func attr(_ attrName: AttrName) -> Zipper? {
        fatalError("TODO")
    }

    /// Move focus "down or to the the right" in such a way that, when called repeatedly starting at the root, all nodes are visited in
    /// pre-order (that is, parents followed by children.)
    public func nextInPreorder() -> Zipper? {
        func findNext(_ loc: Zipper, tryDown: Bool) -> Zipper? {
            if tryDown, let child = loc.down() {
                return child
            }
            else if let sibling = loc.next() {
                return sibling
            }
            else if let parent = loc.up() {
                return findNext(parent, tryDown: false)
            }
            else {
                return nil
            }
        }
        return findNext(self, tryDown: true)
    }

    /// Sequence of all locations within the tree, starting from the root, in pre-order (according to `attrOrder`).
    public func all() -> ZipperSequence {
        return ZipperSequence(nextLoc: root())
    }

    /// Stateful iterator over all the locations in the tree. See `nextInPreorder.`
    public struct ZipperSequence: Sequence, IteratorProtocol {
        var nextLoc: Zipper?

        public mutating func next() -> Zipper? {
            if let result = nextLoc {
                nextLoc = result.nextInPreorder()
                return result
            }
            else {
                return nil
            }
        }
    }

    /// The attribute values which are nodes, ordered according to `attrOrder`.
    private func orderedChildNodes(_ attrs: ([AttrName: Node.Value])) -> [(AttrName, Node)] {
        return attrs.compactMap { name, value in
            if case .Node(let node) = value {
                return (name, node)
            }
            else {
                return nil
            }
        }
        .sorted(by: { p1, p2 in attrOrder(node.type, p1.0, p2.0) })
    }

// MARK: - Mutation
    /*
     Of course none of these methods truly mutate anything, since both Zippers and the underlying
     Nodes are immutable. Therefore it's a question of style whether to use `mutating` functions
     (forcing the use of `var`) or just build new Zipper values and return them.

     For now, following the Swift library's practice for its immutable structs.
     */

    /// Replace the node at the location (and its descendants) entirely.
    public mutating func replace(_ newNode: Node) {
        node = newNode
    }

    /// Replace the NodeId at the location with a newly-generated id, which will therefore be unique with respect to all ids currently
    /// present.
    public mutating func resetId() {
        replace(Node(IdGen.Shared.generateId(), node.type, node.content))
    }

    /// Modify the NodeType at the location, preserving the id and content.
    public mutating func setType(_ newType: NodeType) {
        replace(Node(node.id, newType, node.content))
    }

    /// Works only on .Attrs locations.
    public mutating func setAttr(_ attr: AttrName, _ newValue: Node.Value) {
        if case .Attrs(let attrs) = node.content {
            var newAttrs = attrs
            newAttrs[attr] = newValue
            replace(Node(node.id, node.type, .Attrs(newAttrs)))
        }
    }

    /// Works only on .Ref locations.
    public mutating func setTarget(_ newTarget: NodeId) {
        fatalError("TODO")
    }

    /// Insert a new sibling before the current location, and move the focus to the new node. Works only on .Elems locations.
    public mutating func insertBefore(_ newNode: Node) {
        fatalError("TODO")
    }

    /// Insert a new sibling after the current location, and move the focus to the new node. Works only on .Elems locations.
    public mutating func insertAfter(_ newNode: Node) {
        fatalError("TODO")
    }

    /// TODO: where to leave the location? previous sibling; next sibling; parent?
    public mutating func delete() {
        fatalError("TODO")
    }

    /// Delete the value under a certain attribute name at the location. Note, this is the only way to delete a non-Node value; if the
    /// Works only on .Attrs locations.
    public mutating func deleteAttr(_ attr: AttrName) {
        fatalError("TODO")
    }
}
