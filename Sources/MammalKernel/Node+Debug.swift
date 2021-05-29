extension Node {
    /// A reasonably human-readable, fairly compressed, nicely indented, mostly unambiguous, string representation.
    public var prettyPrint: String {
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

extension Node: CustomReflectable {
    /// Somewhat flattened representation for `dump`. The type and id are in `debugDescription`, so only the content needs to
    /// appear as "children". This is still not as compressed as `Node.prettyPrint`. On the other hand, this is a little more
    /// explicit and unambiguous.
    public var customMirror: Mirror {
        switch content {
        case .Attrs(let attrs):
            return Mirror(self,
                          children: attrs.map { name, value in
                            let name = name.fullName.remove(prefix: self.type.fullName + "/")
                            return (String(name), value)
                          })

        case .Elems(let elems):
            return Mirror(self, unlabeledChildren: elems)

        case .Ref(let target):
            return Mirror(self, children: ["target": target])

        case .Empty:
            return Mirror(self, children: [], displayStyle: .none)
        }
    }
}

extension Node: CustomStringConvertible {
    /// This is what you get with string interpolation or just `print`.
    public var description: String {
        return prettyPrint
    }
}

extension Node: CustomDebugStringConvertible {
    /// This is the label used in `dump`, so it needs to be one line.
    public var debugDescription: String {
        "\(type.fullName) [\(id.id)]"
    }
}


extension NodeId: CustomDebugStringConvertible {
    public var debugDescription: String {
        "NodeId(\(id))"
    }
}
extension NodeId: CustomReflectable {
    public var customMirror: Mirror {
        Mirror(self, unlabeledChildren: [id])
    }
}


extension NodeType: CustomDebugStringConvertible {
    public var debugDescription: String {
        fullName
    }
}
