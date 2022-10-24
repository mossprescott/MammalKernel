extension Node {
    /// "Rename" all the nodes in a tree, so they all have fresh ids that aren't used by *any* previously-existing node.
    ///
    /// *Bound* refs are updated to refer to the same node in the new tree byt the new id. *Unbound* (free) refs are presumed to
    /// refer to some node defined elsewhere, and are left as is.
    public func renameAll(idGen: IdGen = IdGen.Shared) -> Node {
        let newIdsByOldId: [NodeId: NodeId] = Node.Util.descendantsById(of: self)
            .mapValues { _ in idGen.generateId() }
        
        func go(_ node: Node) -> Node {
            var newContent: Node.Content
            
            switch node.content {
            case .Attrs(let attrs):
                newContent = .Attrs(attrs.mapValues { val in
                    switch val {
                    case .Prim(_):
                        return val
                    case .Node(let node):
                        return .Node(go(node))
                    }
                })
                
            case .Elems(let childNodes):
                newContent = .Elems(childNodes.map(go))
                
            case .Ref(let targetId):
                if let newTargetId = newIdsByOldId[targetId] {
                    newContent = .Ref(newTargetId)
                }
                else {
                    newContent = .Ref(targetId)
                }
                
            case .Empty:
                newContent = .Empty
            }
            
            return Node(newIdsByOldId[node.id]!, node.type, newContent)
        }
        
        return go(self)
    }
}
