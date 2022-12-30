import XCTest
import MammalKernel

class SimpleTrieTests: XCTestCase {
    func testEmpty() throws {
        let map = SimpleTrie<String>()

        XCTAssertNil(map[0])
        XCTAssertEqual(map.count, 0)
        XCTAssertEqual(map.capacity(), 32)
    }

    func testOne() throws {
        var map = SimpleTrie<String>()
        map[0] = "abc"

        XCTAssertEqual(map[0], "abc")
        XCTAssertEqual(map.count, 1)
        XCTAssertEqual(map.capacity(), 32)
    }

    func testBigOne() throws {
        var map = SimpleTrie<String>()
        map[100] = "abc"

        XCTAssertEqual(map[100], "abc")
        XCTAssertEqual(map.count, 1)
        XCTAssertEqual(map.capacity(), 1024)
    }

    func testMany() throws {
        var map = SimpleTrie<Int>()

        for i: UInt in 0..<1000 {
            map[i] = Int(i)
        }

        for i: UInt in 0..<1000 {
            XCTAssertEqual(map[i], Int(i))
        }

        XCTAssertNil(map[1000])

        XCTAssertEqual(map.count, 1000)
        XCTAssertEqual(map.capacity(), 1024)
    }

// MARK: Persistence

    func testPersistent() throws {
        var map1 = SimpleTrie<String>()
        map1[0] = "abc"
        map1[1] = "def"

        var map2 = map1
        map2[1] = "ghi"

        // map1 is unchanged after a "copy" of it is "modified":
        XCTAssertEqual(map1[0], "abc")
        XCTAssertEqual(map1[1], "def")

        XCTAssertEqual(map2[0], "abc")
        XCTAssertEqual(map2[1], "ghi")
    }

    func testFarLookup() throws {
        var map = SimpleTrie<String>()
        map[0] = "abc"

        let initialCapacity = map.capacity()

        let missing = map[UInt(initialCapacity*10 + 13)]

        XCTAssertNil(missing)
        XCTAssertEqual(map.count, 1)

        // The capacity is unaffected by a lookup:
        XCTAssertEqual(map.capacity(), initialCapacity)
    }


// MARK: Performace

    /// Write a large number of values, starting with an empty map.
    ///
    /// Best performance: 4-6 bits
    func testWritePerformance() throws {
        var map = SimpleTrie<Int>()

        self.measure {
            for i: UInt in 0..<100_000 {
                map[i] = Int(i)
            }
        }

        print("capacity: \(map.capacity())")
    }

    /// Write a large number of values, spread out so there's only one or two values per leaf node, starting with an empty map.
    ///
    /// Best performance: 5 bits
    func testSparseWritePerformance() throws {
        var map = SimpleTrie<Int>()

        self.measure {
            for i: UInt in 0..<100_000 {
                map[11*i] = Int(i)
            }
        }

        print("capacity: \(map.capacity())")
    }

    /// Read from successive indexes, repeating the whole sequence several times.
    ///
    /// Best performance: pretty flat for 5-9 bits.
    func testReadPerformance() throws {
        var map = SimpleTrie<Int>()
        for i: UInt in 0..<1024 {
            map[i] = Int(i)
        }

        var sum: Int = 0

        self.measure {
            for i: UInt in 0..<200*1024 {
                sum += map[i % 1024] ?? 0
            }
        }

        print("sum: \(sum)")
        print("capacity: \(map.capacity())")
    }
}
