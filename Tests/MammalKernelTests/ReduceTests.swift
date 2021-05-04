import XCTest
import MammalKernel

final class ReduceTests: XCTestCase {
    let kernel = KernelGen()

    let fooType = NodeType("test", "foo")
    let barType = NodeType("test", "bar")
    let bazType = NodeType("test", "baz")
    let childAttr = AttrName(fullName: "test/child")

    func testNoOp() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)

        let reduction = Reduce.TopDown([:])

        let (result, sourceMap) = reduction.reduce(pgm)

        XCTAssertEqual(result.id, pgm.id)
        XCTAssertEqual(result.type, pgm.type)

        try checkInvariants(pgm: pgm, result: result, sourceMap)
    }

    /// foo -> bar
    func testSimpleReplace() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)

        let reduction = Reduce.TopDown(
            [
                fooType: trivialReduceFn(barType),
            ])

        let (result, sourceMap) = reduction.reduce(pgm)

        XCTAssertNotEqual(result.id, pgm.id)
        XCTAssertEqual(result.type, barType)
        XCTAssertEqual(sourceMap.sourceIds[result.id], pgm.id)

        try checkInvariants(pgm: pgm, result: result, sourceMap)
    }

    /// A single child goes from foo -> bar.
    func testReplaceChild() throws {
        let foo = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)
        let pgm = Node(IdGen.Shared.generateId(),
                       NodeType("test", "fooParent"),
                       .Attrs([
                        childAttr: .Node(foo)
                       ]))

        let reduction = Reduce.TopDown(
            [
                fooType: trivialReduceFn(barType),
            ])

        let expected = Node(IdGen.Shared.generateId(),
                            NodeType("test", "fooParent"),
                            .Attrs([
                                childAttr: .Node(
                                    Node(IdGen.Shared.generateId(),
                                         barType,
                                         .Empty)),
                            ]))

        let (result, sourceMap) = reduction.reduce(pgm)

        print("pgm:\n\(pgm.debugDescription)")
        print("expected:\n\(expected.debugDescription)")
        print("result:\n\(result.debugDescription)")
        sourceMap.sourceIds.forEach { out, src in
            print("\(out) -> \(src)")
        }

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])

        XCTAssertEqual(
            Node.Util.children(of: result).map { child in
                sourceMap.sourceIds[child.id]
            },
            Node.Util.children(of: pgm).map(\.id),
            "the child maps to the corresponding source node")

        XCTAssertNotEqual(result.id, pgm.id,
                          "the rebuilt root node gets a fresh id")
        XCTAssertEqual(sourceMap.sourceIds[result.id], pgm.id,
                      "the rebuit root node maps to the original root")

        try checkInvariants(pgm: pgm, result: result, sourceMap)
    }

    /// Two children going from foo -> bar side-by-side.
    func testParallelReplace() throws {
        let foo1 = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)
        let foo2 = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)
        let pgm = Node(IdGen.Shared.generateId(),
                       NodeType("test", "fooParent"),
                       .Elems([foo1, foo2]))

        let reduction = Reduce.TopDown(
            [
                fooType: trivialReduceFn(barType),
            ])

        let expected = Node(IdGen.Shared.generateId(),
                            NodeType("test", "fooParent"),
                            .Elems([
                                Node(IdGen.Shared.generateId(),
                                     barType,
                                     .Empty),
                                Node(IdGen.Shared.generateId(),
                                     barType,
                                     .Empty)
                            ]))

        let (result, sourceMap) = reduction.reduce(pgm)

        print("pgm:\n\(pgm.debugDescription)")
        print("expected:\n\(expected.debugDescription)")
        print("result:\n\(result.debugDescription)")
        sourceMap.sourceIds.forEach { out, src in
            print("\(out) -> \(src)")
        }

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])

        // Each child maps to the corresponding source node (even though they
        // resulted from the same quoted node):
        XCTAssertEqual(
            Node.Util.children(of: result).map { child in
                sourceMap.sourceIds[child.id]
            },
            Node.Util.children(of: pgm).map(\.id))

        XCTAssertEqual(sourceMap.sourceIds[result.id], pgm.id)

        try checkInvariants(pgm: pgm, result: result, sourceMap)
    }

    // foo -> bar -> baz
    func testChain() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)

        let reduction = Reduce.TopDown(
            [
                fooType: trivialReduceFn(barType),
                barType: trivialReduceFn(bazType),
            ])

        let (result, sourceMap) = reduction.reduce(pgm)

        XCTAssertNotEqual(result.id, pgm.id)
        XCTAssertEqual(result.type, bazType)
        XCTAssertEqual(sourceMap.sourceIds[result.id], pgm.id)

        try checkInvariants(pgm: pgm, result: result, sourceMap)
    }

    // TODO: some source node gets duplicated via unquote

    // TODO: reduction loops infinitely (and gets interrupted)


    private func checkInvariants(pgm: Node,
                         result: Node,
                         _ sourceMap: Reduce.SourceMap,
                         file: StaticString = #filePath,
                         line: UInt = #line) throws {


        print("pgm:\n\(pgm.debugDescription)")
        print("result:\n\(result.debugDescription)")
        print("sourceMap:")
        sourceMap.sourceIds.forEach { out, src in
            print("  \(out) -> \(src)")
        }

//        XCTAssertTrue(Set(sourceMap.sourceIds.values),
//                       Set(Node.Util.descendants(of: pgm).keys),
//                       "every source node must appear in sourceMap (as a value)",
//                       file: file, line: line)

        let sourceIds = Set(sourceMap.sourceIds.values)
        let pgmNodeIds = Set(Node.Util.descendants(of: pgm).keys)
        XCTAssertTrue(sourceIds.isSubset(of: pgmNodeIds),
                       "every value of sourceMap is a node in the source tree",
                       file: file, line: line)

        let reducedRootIds = Set(sourceMap.sourceIds.keys)
        let outputNodeIds = Set(Node.Util.descendants(of: result).keys)
        XCTAssertTrue(reducedRootIds.isSubset(of: outputNodeIds),
                       "every key of sourceMap is a node in the output tree",
                       file: file, line: line)

        XCTAssertTrue(reducedRootIds.isDisjoint(with: pgmNodeIds),
                      "every key of sourceMap is a new node (doesn't appear in the source)")

        // TODO: output is well-formed:
        // - no duplicate ids (... unless they are replicated source nodes)
        // - no dangling refs (...?)
    }

    private func trivialReduceFn(_ type: NodeType) -> Node {
        kernel.Lambda(arity: 1) { _, _ in
            kernel.Quote(body:
                            Node(IdGen.Shared.generateId(),
                                 type,
                                 .Empty))
        }
    }

    static var allTests = [
        ("testNoOp", testNoOp),
        ("testSimpleReplace", testSimpleReplace),
        ("testParallelReplace", testParallelReplace),
        ("testChain", testChain),
    ]
}