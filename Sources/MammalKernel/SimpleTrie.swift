import Foundation

/// Constants controlling the branch factor, which is always a power of 2. Smaller values mean allocating more, smaller tables, and
/// a (slightly) deeper tree. Larger values mean a shallower tree, and fewer, larger table allocations.
///
/// 32 is the conventional choice, giving two-level trees of up to 1024 keys, four-level trees up to a million.
///
/// For our purposes, trees probably have only tens or hundreds of keys, and they're compact, so we'll have 1-2 levels regardless.
@usableFromInline
internal let BITS: UInt = 5

@usableFromInline
internal let TABLE_SIZE: UInt = 1 << BITS


/// Array-mapped Trie data structure, for mapping integer keys to arbitrary values, with efficient, non-destructive updates.
///
/// - the keys can be any set of non-negative integers, not necessarily starting at 0
/// - no "remove" operation is provided; it would be a little bit involved to implement while guaranteeing the result is minimal
/// - for best performance, use a "compact" set of keys; that is, a sequence of values with few unused keys mixed in
///
/// Performance
/// - the large branch factor means the tree is never more than a few levels deep
/// - lookups are effectively O(1)
/// - updates do O(1) allocations
public struct SimpleTrie<Value> {

    @usableFromInline
    var node: Node

    init() {
        node = SimpleTrie<Value>.empty()
    }

    /// The number of entries the trie is able to store without increasing its depth (and therefore its memory footprint.) Equivalently,
    /// one more than the maximum key that can be associated with a value without increasing the memory footprint.
    ///
    /// O(1)
    public func capacity() -> Int {
        switch node {
        case .Leaf(_):
            return Int(TABLE_SIZE)
        case .NonLeaf(let level, _):
            return 1 << (BITS*(level+1))
        }
    }

    ///
    @usableFromInline
    enum Node {
        case Leaf([Value?])
        case NonLeaf(level: UInt, [Node])
    }

    /// A empty table, used to initialize any new node. Ideally would be shared.
    @usableFromInline
    static func empty() -> Node { Node.Leaf(Array(repeating: nil, count: Int(TABLE_SIZE))) }
}

extension SimpleTrie {
//    @inlinable
    public subscript(key: UInt) -> Value? {
        get {
            guard key < capacity() else {
                return nil
            }

            func find(_ idx: UInt, _ node: Node) -> Value? {
                switch node {
                case .Leaf(let table):
                    return table[Int(idx)]
                case .NonLeaf(let level, let children):
                    let indexes = indexes(level: level, idx)
                    return find(indexes.inner, children[Int(indexes.outer)])
                }
            }

            return find(UInt(key), node)
        }

        set {
            // Extend the structure to the required depth:
            while key >= capacity() {
                var currentLevel: UInt
                switch node {
                case .Leaf(_):
                    currentLevel = 0
                case .NonLeaf(let level, _):
                    currentLevel = level
                }

                // Create a chain of nodes all referring to the same, empty Leaf at the bottom:
                func emptyChain(depth: UInt) -> Node {
                    if depth > 0 {
                        let next = emptyChain(depth: depth-1)
                        return .NonLeaf(level: depth, Array(repeating: next, count: Int(TABLE_SIZE)))
                    }
                    else {
                        return SimpleTrie<Value>.empty()
                    }
                }
                let chain = emptyChain(depth: currentLevel)
                node = .NonLeaf(level: currentLevel + 1, [node] + Array(repeating: chain, count: Int(TABLE_SIZE)-1))
            }

            // Construct a new node by replacing a single value in one of the leaves, building a new
            // node at each intermediate level:
            func rebuild(_ idx: UInt, _ node: Node) -> Node {
                switch node {
                case .Leaf(let table):
                    var newTable = table
                    newTable[Int(idx)] = newValue
                    return .Leaf(newTable)

                case .NonLeaf(let level, let children):
                    let indexes = indexes(level: level, idx)
                    var newChildren = children
                    newChildren[Int(indexes.outer)] = rebuild(indexes.inner, newChildren[Int(indexes.outer)])
                    return .NonLeaf(level: level, newChildren)
                }
            }

            node = rebuild(UInt(key), node)
        }
    }

    @inlinable
    func indexes(level: UInt, _ idx: UInt) -> (outer: UInt, inner: UInt) {
        let innerBits = level*BITS
        return (outer: idx >> innerBits,
                inner: idx & ((1 << innerBits) - 1))
    }
}

extension SimpleTrie: Sequence {
    /// Count the number of (non-nil) values, in the most obvious and brain-dead way.
    ///
    /// O(n)
    ///
    /// Note: a more efficient implementation is possible, by tracking counts in the nodes, or just by keeping track of empty tables.
    public var count: Int {
        func go(_ node: Node) -> Int {
            switch node {
            case .Leaf(let table):
                return table.reduce(0) { (acc, val) in acc + (val != nil ? 1 : 0) }
            case .NonLeaf(_, let nodes):
                return nodes.reduce(0) { (acc, child) in acc + go(child) }
            }
        }
        return go(node)
    }

    public func makeIterator() -> Iterator {
        return Iterator(self)
    }

    public class Iterator: IteratorProtocol {
        let trie: SimpleTrie
        var nextIndex: UInt = 0

        init(_ trie: SimpleTrie) {
            self.trie = trie
        }

        public func next() -> (key: UInt, value: Value)? {
            let capacity = trie.capacity()

            // skip empty slots:
            while nextIndex < capacity && trie[nextIndex] == nil {
                nextIndex += 1
            }
            if nextIndex < capacity, let val = trie[nextIndex] {
                let result = (key: nextIndex, value: val)
                nextIndex += 1
                return result
            }
            else {
                return nil
            }
        }
    }
}

extension SimpleTrie {
    /// O(n)
    public func mapValues<T>(_ transform: (Value) throws -> T) rethrows -> SimpleTrie<T> {
        var result = SimpleTrie<T>()
        for (k, v) in self {
            result[k] = try transform(v)
        }
        return result
    }
}

extension SimpleTrie: ExpressibleByDictionaryLiteral {
    public typealias Key = UInt

    public init(dictionaryLiteral elements: (UInt, Value)...) {
        var tr = SimpleTrie()
        for (k, v) in elements {
            tr[k] = v
        }
        node = tr.node
    }
}

// MARK: - Debugging aids

extension SimpleTrie: CustomDebugStringConvertible {
    public var debugDescription: String {
        (["["] + Array(self).map { (k, v) in "\(k): \(v)," } + ["]"]).joined(separator: ", ")
    }
}

extension SimpleTrie: CustomReflectable {
    public var customMirror: Mirror {
//        let nonNil: Dictionary<String?, Value> = Dictionary(uniqueKeysWithValues: )
        Mirror(self, children: self.map { (k, v) in (String(k), v)}, displayStyle: .dictionary)
    }
}
