/// Namespace for *reductions*, which are ways of transforming some input program (a tree of Nodes) into an output program (another
/// tree of Nodes), by systematically applying some set of transformations.
///
/// Transformations are typically `kernel`-language programs which consume a `Node` at a time and produce a `Node` as a result.
/// A reduction strategy is provided with some transformations and applies them in a particular way to achieve the desired effect.
/// Different strategies will be useful for different kinds of overall transformations.
///
/// This is where the meta-programming rubber meets the road, so it gets a bit involved. The guiding principle is that applying a
/// reduction results in some kind of useful value, even if the input is unexpected or component parts that make up the reduction behave
/// badly. Therefore, the generalized reduction strategy(-ies) provided here are very defensively defined and implemented. Providing that
/// kind of safety for a few different reduction strategies allows many different *transformations* to be written in the most simple, direct
/// way (i.e. as trivial kernel lambdas); when they're applied by one of these reduction strategies, all the potential pitfalls are handled
/// for you.
public enum Reduce {

    /// Generalized reduction function. Consumes an arbitrary input and applies some predefined transformations, yielding an output,
    /// plus a mapping which records what nodes in the output arose from which nodes in the input.
    ///
    /// - Invariant
    ///     - *reduction terminates*: if reduction is unable to complete normally for some reason, it bails out producing some
    ///       kind of partially-reduced result.
    ///     - *source map is comprehensive*: the source map contains enough information to trace the origin of every node in the
    ///       output.
    ///     - *source map is free of cruft*: *only* nodes that appear in the result are in the source map
    public typealias Reduction = (Node) -> (Node, SourceMap)

    /// Associates output nodes with the source program nodes they arose from, by way of a simple dictionary of node ids.
    public struct SourceMap {
        /// Each key is the id of a unique node in the output program; the value is the id of the source node from which the node was
        /// derived. That is, each time a reduction consumes a source node, the id of the node which is the root of the resulting
        /// sub-tree is recorded.
        ///
        /// Note: the relation is many-to-one: it's expected that reduction will often result in several output nodes mapping back the
        /// to the same source node, but there is always a unique source node identified for each output node.
        ///
        ///
        public var sourceIds: [NodeId: NodeId]
    }

    /// A collection of values that will be made available when reductions are being evaluated. To refer to one of them, just
    /// embed a node with the appropriate type; the content is ignored. See `KernelGen.Constant()`.
    public struct Library {
        var buildValues: (Node) -> [NodeType: Eval.Value<Node.Value>]

        private init(buildValues: @escaping (Node) -> [NodeType: Eval.Value<Node.Value>]) {
            self.buildValues = buildValues
        }

        /// Library providing only `resolveRef`.
        public static let resolver = Library { root in
            [Lib.resolveRef: makeResolver(within: root)]
        }

        /// Overlay additional context-insensitive values.
        public func extend(with values: [NodeType: Eval.Value<Node.Value>]) -> Library {
            Library { root in
                self.buildValues(root).merging(values, uniquingKeysWith: { $1 })
            }
        }
    }

    /// Constants for referring to values which are available during evaluation of reductions.
    public enum Lib {
        private static let namespace = "reduce-lib"  // TODO: figure out naming

        /// Resolve a reference to a node that's "in scope". Note: this can't be implemented statically, but has to be provided
        public static let resolveRef = NodeType(namespace, "resolveRef")
    }

    /// The best and most well-supported form of reduction, which works on the tree in single-pass, top-down fashion, and
    /// uses the node type to look up a single, authoritative reduction function.
    public struct TopDown {
        /// A kernel language program for each reducible node type. Each defines a unary function which will be called with
        /// a node of the corresponding type. If the node is understood, the function returns an `expr` Node. If not, `nil` is returned
        /// and it's up to the caller to figure out what to display. The result may contain descendant nodes which are not yet reduced.
        public var reducers: [NodeType: Node]

        /// A library of values which are available when each reduction is evaluated.
        public var library: Library

        /// A hook to avoid hard-coding the NodeId generator.
        var generateId = IdGen.Shared.generateId

        // TODO: some kind of overridable handler for each error case that can be trapped

        public init(_ reducers: [NodeType: Node], library: Library = Library.resolver) {
            self.reducers = reducers
            self.library = library
        }

        /// Run reducers, starting at the root node, and then processing children recursively.
        /// As long as the result node's type is found in `reducers`, it will be repeatedly reduced.
        /// When the the node is no longer reduced (either there's no matching reducer, or the reducer returns `nil`),
        /// proceed with the children.
        ///
        /// Reducers can return any well-formed tree. It is the responsibility of this function to ensure that the results are
        /// assembled into a well-formed tree (for example, by re-labeling nodes to avoid id collisions.) TODO: what about "free" ref
        /// nodes?
        ///
        /// TODO: some kind of arbitrary limit on how much reduction/expansion can happen, to
        /// interrupt non-terminating reductions.
        public func reduce(_ root: Node) -> (Node, SourceMap) {
            let lib = library.buildValues(root)

            let allSourceIds = Set(Node.Util.descendants(of: root).keys)
            var reducedRootToSourceId: [NodeId: NodeId] = [:]
            func record(_ from: Node, reducedTo to: Node) {
                if let previousResult = reducedRootToSourceId[from.id] {
                    reducedRootToSourceId.removeValue(forKey: from.id)
                    reducedRootToSourceId[to.id] = previousResult
                }
                else if allSourceIds.contains(from.id) {
                    reducedRootToSourceId[to.id] = from.id
                }
            }

            // If there is a reduction for the root node's type, run it and if it succeeds, then
            // return the result. If the reduction returns `nil` or a failure value, or actually
            // throws, it's as if no reduction was found.
            func reduceNode(_ node: Node) -> Node? {
                if let f = reducers[node.type] {
                    let kernel = KernelGen()
                    let pgm = kernel.App(fn: f, args: [
                        kernel.Quote(body: node)
                    ])
                    do {
                        let result = try Kernel.eval(pgm, constants: lib)
                        if result.type == Kernel.Nil.type {
                            print("Reduction returned nil: \(node)")
                            return nil
                        } else if result.type == Kernel.Fail.type {
                            print("Reduction returned an error: \(result)")
                            return nil
                        } else {
                            record(node, reducedTo: result)
                            return result
                        }
                    }
                    catch {
                        print("Reduction threw an exception: \(error)")
                        return nil
                    }
                }
                else {
                    return nil
                }
            }

            // Apply `reduce` to each child node and build up a new node with the results.
            func reduceChildren(_ node: Node) -> Node {
                // TODO: detect when no reduction happens down the tree and avoid constructing
                // new nodes when it's not necessary.

                switch node.content {
                case .Attrs(let attrs):
                    let reducedAttrs: Node.Content = .Attrs(
                        attrs.mapValues { val in
                            switch val {
                            case .Prim(_):
                                return val
                            case .Node(let child):
                                return .Node(loop(child))
                            }
                        })
                    let rebuilt = Node(generateId(), node.type, reducedAttrs)
                    record(node, reducedTo: rebuilt)
                    return rebuilt

                case .Elems(let elems):
                    let reducedElems: Node.Content = .Elems(elems.map(loop))
                    let rebuilt = Node(generateId(),
                                       node.type,
                                       reducedElems)
                    record(node, reducedTo: rebuilt)
                    return rebuilt

                case .Ref(_), .Empty:
                    return node
                }
            }

            func loop(_ node: Node) -> Node {
                if let reduced = reduceNode(node) {
                    return loop(reduced)
                }
                else {
                    return reduceChildren(node)
                }
            }

            let reducedRoot = loop(root)

            return (reducedRoot, SourceMap(sourceIds: reducedRootToSourceId))
        }
    }

    // TODO: OneTime reduction, which is allowed to embed the source in its output without exploding



// MARK: -Internals

    private static func makeResolver(within root: Node) -> Eval.Value<Node.Value> {
        let nodesById = Node.Util.descendants(of: root)

        return Eval.Value.Fn(arity: 1) { args in
            switch args[0] {
            case .Val(.Node(let node)):
                switch node.content {
                case .Ref(let target):
                    if let targetNode = nodesById[target] {
                        return .Val(.Node(targetNode))
                    }
                    else {
                        return .Val(.Prim(.Nil))
                    }
                default:
                    return .Val(.Prim(.Nil))
                }
            default:
                return .Error(.TypeError(expected: "ref", found: args[0]))
            }
        }
    }

}
