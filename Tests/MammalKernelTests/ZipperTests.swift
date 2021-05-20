import XCTest
import MammalKernel

final class ZipperTests: XCTestCase {

    func testSingleton() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Empty)

        let zipper = Zipper(pgm, attrOrder: byName)

        let expected = pgm

        let result = zipper.root()

        XCTAssertEqual(Diff.changes(from: expected, to: result), [])

        XCTAssertNil(zipper.previous())
        XCTAssertNil(zipper.next())
        XCTAssertNil(zipper.up())
        XCTAssertNil(zipper.down())
    }

    func testSimpleElems() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Elems([
                        Node(IdGen.Shared.generateId(),
                             barType,
                             .Empty),
                        Node(IdGen.Shared.generateId(),
                             bazType,
                             .Empty),
                       ]))

        let fooLoc = Zipper(pgm, attrOrder: byName)

        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc.root()), [])
        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc.node), [])

        let barLoc = fooLoc.down()!
        XCTAssertEqual(barLoc.node.type, barType)

        let bazLoc = barLoc.next()!
        XCTAssertEqual(bazLoc.node.type, bazType)

        let fooLoc2 = bazLoc.up()!
        XCTAssertEqual(fooLoc2.node.type, fooType)

        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc2.node), [])

        let barLoc2 = bazLoc.previous()!
        XCTAssertEqual(barLoc2.node.type, barType)

        XCTAssertEqual(Diff.changes(from: pgm, to: barLoc2.root()), [])
    }

    func testSimpleAttrs() throws {
        let pgm = Node(IdGen.Shared.generateId(),
                       fooType,
                       .Attrs([
                        AttrName(fooType, "bar"):
                            .Node(Node(IdGen.Shared.generateId(),
                                       barType,
                                       .Empty)),
                        AttrName(fooType, "baz"):
                            .Node(Node(IdGen.Shared.generateId(),
                                       bazType,
                                       .Empty)),
                        AttrName(fooType, "quux"):
                            .Prim(.Nil),
                     ]))

        let fooLoc = Zipper(pgm, attrOrder: byName)

        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc.root()), [])
        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc.node), [])

        let barLoc = fooLoc.down()!
        XCTAssertEqual(barLoc.node.type, barType)

        let bazLoc = barLoc.next()!
        XCTAssertEqual(bazLoc.node.type, bazType)

        let fooLoc2 = bazLoc.up()!
        XCTAssertEqual(fooLoc2.node.type, fooType)

        XCTAssertEqual(Diff.changes(from: pgm, to: fooLoc2.node), [])

        let barLoc2 = bazLoc.previous()!
        XCTAssertEqual(barLoc2.node.type, barType)

        XCTAssertEqual(Diff.changes(from: pgm, to: barLoc2.root()), [])
    }

    let fooType = NodeType("test", "foo")
    let barType = NodeType("test", "bar")
    let bazType = NodeType("test", "baz")

    let byName: Zipper.AttrsInIncreasingOrder = { _, x, y in
        x.fullName < y.fullName
    }

    static var allTests = [
        ("testSingleton", testSingleton),
    ]
}
