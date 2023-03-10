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
    public typealias Value = Eval.Value<Int, Node.Value>

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
        var reducedIdToSourceId: [NodeId: NodeId]

        /// True if no node was reduced; every node in the result is identical to one in the source.
        public var isEmpty: Bool {
            reducedIdToSourceId.isEmpty
        }

        /// Ids of all the reduced "roots", each of which is the sub-tree which resulted from reducing a single source node.
        public var reducedRootIds: Set<NodeId> {
            Set(reducedIdToSourceId.keys)
        }

        /// Given the id of a node in the reduced result, look up the id of the corresponding node in the source tree. That is, find the
        /// id of the node in the source which was consumed by a `ReduceFn`, the result of which was a "reduced" node having the
        /// presented id.
        public func sourceId(forReducedId id: NodeId) -> NodeId? {
            reducedIdToSourceId[id]
        }
    }

    /// A collection of values that will be made available when reductions are being evaluated. To refer to one of them, just
    /// embed a node with the appropriate type; the content is ignored. See `KernelGen.Constant()`.
    public struct Library {
        var buildValues: (Node) -> [NodeType: Value]

        private init(buildValues: @escaping (Node) -> [NodeType: Value]) {
            self.buildValues = buildValues
        }

        /// Library providing only `resolveRef`.
        public static let resolver = Library { root in
            [Lib.resolveRef: makeResolver(within: root)]
        }

        /// Overlay additional context-insensitive values.
        public func extend(with values: [NodeType: Value]) -> Library {
            Library { root in
                self.buildValues(root).merging(values, uniquingKeysWith: { $1 })
            }
        }
    }

    /// Constants for referring to values which are available during evaluation of reductions.
    public enum Lib {
        private static let namespace = "reduce-lib"  // TODO: figure out naming

        /// Resolve a reference to a node that's "in scope". For now, that just means that it
        /// exists in the (source) tree that's being reduced.
        public static let resolveRef = NodeType(namespace, "resolveRef")
    }

    /// A function which may or may not "reduce" a single node at a time.
    ///
    /// Note: the result node should always contain all "fresh" node ids, to avoid id collisions when the function is applied to another
    /// node. That's not very FP, but it's what we've got for now. If the fn is actually a `kernel/lambda` being applied to the node,
    /// and expanding a `kernel/quote` to produce its result, then this is taken care of by the node re-naming that happens during
    /// quote expansion.
    public typealias ReduceFn = (Node, Context) throws -> Node?

    /// Additional information that's provided to the reducing function along with the node to be
    /// reduced.
    public struct Context {
        /// The root of the tree containing the node to be reduced.
        var rootNode: Node
    }

    /// No-op reduction; just constructs a valid response with no nodes affected.
    public struct None {
        public init() {}
        
        public func reduce(_ root: Node) -> (Node, SourceMap) {
            return (root, SourceMap(reducedIdToSourceId: [:]))
        }
    }

// MARK: -TopDown

    /// Apply a reduce function to the nodes of a tree in single-pass, top-down fashion.
    public struct TopDown {
        /// Reduce function to be applied to each node starting at the root. If the node is
        /// understood, the function returns a new Node which is substituted into the tree, and
        /// recorded in the `SourceMap`. The function is applied repeatedly until it no longer
        /// performs any reduction before proceeding.
        public var reduceFn: ReduceFn

        /// A hook to avoid hard-coding the NodeId generator.
        var generateId = IdGen.Shared.generateId

        // TODO: some kind of overridable handler for each error case that can be trapped

        public init(_ reduceFn: @escaping ReduceFn) {
            self.reduceFn = reduceFn
        }

        /// Run the reduceFn starting at the root node and then processing children recursively.
        /// As long as a result is produced, the reduceFn is called repeatedly.
        /// When the the node is no longer reduced (the reduceFn returns `nil` or fails),
        /// proceed with the children.
        ///
        /// The reduceFn should produce nodes that result in a well-formed tree when they are
        /// assembled along with unreduced source nodes. For example, separate calls to reduce
        /// different nodes should result in results with distinct node ids, so that the result
        /// is free of ambiguously-labeled nodes.
        ///
        /// TODO: some kind of arbitrary limit on how much reduction/expansion can happen, to
        /// interrupt non-terminating reductions.
        public func reduce(_ root: Node) -> (Node, SourceMap) {
            let context = Context(rootNode: root)

            let allSourceIds = Set(Node.Util.descendantsById(of: root).keys)
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

            // Run the reduce function on the node. If it succeeds, then record the new node's id
            // and return it. If the reduction returns `nil` or throws, no more reduction is possible,
            // and nil is returned..
            func reduceNode(_ node: Node) -> Node? {
                do {
                    if var result = try reduceFn(node, context) {
                        // The result node might occur more than once, and we need to track each one
                        // independently (e.g. multiple `nil`s in the source program, each reduced
                        // to the same string for display). So just relabel the root no matter what.
                        result.id = generateId()
                        record(node, reducedTo: result)
                        return result
                    }
                    else {
                        return nil
                    }
                }
                catch {
                    print("Reduction threw an exception: \(error); \(node)")
                    return nil
                }
            }

            // Apply `reduce` to each child node and build up a new node with the results. If no
            // reduction occurs down the tree, then nil.
            func reduceChildren(_ node: Node) -> Node? {
                switch node.content {
                case .Attrs(let attrs):
                    let (newAttrs, changed): ([(AttrName, Node.Value)], Bool) = attrs.reduce(([], false)) { previousResult, entry in
                        if case .Node(let child) = entry.value, let newChild = loop(child) {
                            return (previousResult.0 + [(entry.key, .Node(newChild))], true)
                        }
                        else {
                            return (previousResult.0 + [(entry.key, entry.value)], previousResult.1)
                        }
                    }
                    if changed {
                        let rebuilt = Node(generateId(),
                                           node.type,
                                           .Attrs(Dictionary(uniqueKeysWithValues: newAttrs)))
                        record(node, reducedTo: rebuilt)
                        return rebuilt
                    }
                    else {
                        return nil
                    }

                case .Elems(let elems):
                    let (newElems, changed): ([Node], Bool) = elems.reduce(([], false)) { previousResult, child in
                        if let newChild = loop(child) {
                            return (previousResult.0 + [newChild], true)
                        }
                        else {
                            return (previousResult.0 + [child], previousResult.1)
                        }
                    }
                    if changed {
                        let rebuilt = Node(generateId(),
                                           node.type,
                                           .Elems(newElems))
                        record(node, reducedTo: rebuilt)
                        return rebuilt
                    }
                    else {
                        return nil
                    }

                case .Ref(_), .Empty:
                    return nil
                }
            }

            // Reduce at the root of a sub-tree, repeatedly, then proceed with the children. If no
            // reduction happens anywhere, then nil.
            func loop(_ node: Node) -> Node? {
                if let reduced = reduceNode(node) {
                    // Tricky:
                    return loop(reduced) ?? reduced
                }
                else {
                    return reduceChildren(node)
                }
            }

            let reducedRoot = loop(root)

            return (reducedRoot ?? root, SourceMap(reducedIdToSourceId: reducedRootToSourceId))
        }
    }


// MARK: -Utilities

    /// Build a `ReduceFn` out of a `kernel` program for each type; each program is a unary function
    /// which attempts to reduce a node of the associated type.
    public static func reduceByTypeWithKernel(_ reducers: [NodeType: Node], library: Library = Library.resolver) -> ReduceFn {
        { node, context in
            let lib = library.buildValues(context.rootNode)

//            print("lib: \(lib.keys)")

            // Like Kernel.eval, but then match only a unary function result (and yield the raw value)
            func evalToFn1(_ lambdaNode: Node) throws -> ((Node) throws -> Value) {
                let ast = Kernel.translate(lambdaNode, constants: lib)
                // Alternatively: construct an App that calls ast
//                return { n in
//                    Eval.eval(Expr.App(fn: ast, args: [...]))
//                }
                let result = Eval.eval(ast, env: [:])
                switch result {
                case .Closure(_, let params, let body, _):
                    // Note: ignoring any captured environment, because??? ?
                    guard params.count == 1 else {
                        throw Eval.RuntimeError.ArityError(expected: 1, found: [result])  // Abusing the "found" field here
                    }
                    return { n in Eval.eval(body, env: [params[0]: .Val(.Node(n))]) }
                case .NativeFn(arity: 1, let f):
                    return { n in try f([.Val(.Node(n))]) }
                case .NativeFn(_, _):
                    throw Eval.RuntimeError.ArityError(expected: 1, found: [result])  // Abusing the "found" field here
                default:
                    throw Eval.RuntimeError.TypeError(expected: "unary function", found: result)
                }
            }

            if let fnNode = reducers[node.type] {
                let fn = try evalToFn1(fnNode)
                let result = try fn(node)
                switch result {
                case .Val(.Node(let node)):
                    return node

                case .Val(.Prim(.Nil)):
                    return nil

                case .Val(.Prim(_)), .Closure(_, _, _, _), .NativeFn(_, _):
                    throw Eval.RuntimeError.TypeError(expected: "node", found: result)

                case .Error(let err):
                    throw err
                }
            }
            else {
                return nil
            }
        }
    }

    /// Run one reduction, followed by another, tracking sources through both reductions.
    public static func compose(_ f: @escaping Reduction, _ g: @escaping Reduction) -> Reduction {
        return { node in
            let (reduced1, srcMap1) = f(node)
            let (reduced2, srcMap2) = g(reduced1)

            var srcMap = srcMap1.reducedIdToSourceId
            for (rId, sId) in srcMap2.reducedIdToSourceId {
                if let previousSourceId = srcMap1.sourceId(forReducedId: sId) {
                    srcMap.removeValue(forKey: sId)
                    srcMap[rId] = previousSourceId
                }
                else {
                    srcMap[rId] = sId
                }
            }
            let srcMapCombined: Reduce.SourceMap = SourceMap(reducedIdToSourceId: srcMap)

            return (reduced2, srcMapCombined)
        }
    }


// MARK: -Internals

    private static func makeResolver(within root: Node) -> Value {
        let nodesById = Node.Util.descendantsById(of: root)

        return Eval.Value.NativeFn(arity: 1) { args in
            switch args[0] {
            case .Val(.Node(let node)):
                if case .Ref(let target) = node.content,
                   let targetNode = nodesById[target] {
                    return .Val(.Node(targetNode))
                }
                else {
                    return .Val(.Prim(.Nil))
                }
            default:
                return .Error(.TypeError(expected: "ref", found: args[0]))
            }
        }
    }

}
