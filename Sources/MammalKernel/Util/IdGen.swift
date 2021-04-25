import Foundation

/// A source of identifiers that are more or less unique. A single instance will never produce the same id twice.
public class IdGen {
    var nextId = 0

    init(seed: Int) {
        nextId = seed
    }

    public func generateId() -> NodeId {
        let id = NodeId(nextId)
        nextId += 1
        return id
    }

    /// This instance is seeded with the current time (µs precision), so it produces ids that look like they're probably globally unique,
    /// but without any real guarantee.
    public static let Shared: IdGen = {
#if DEBUG
        print("WARNING: generating ids from 0 for testing purposes")
        let seed = 0
#else
        let seed = Int64(Date.timeIntervalSinceReferenceDate*1_000_000)
        print("NodeId seed: \(seed)")
#endif

        return IdGen(seed: seed)
    }()
}
