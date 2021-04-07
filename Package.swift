// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "MammalKernel",
    products: [
        .library(
            name: "MammalKernel",
            targets: ["MammalKernel"]),
    ],
    dependencies: [
    ],
    targets: [
        .target(
            name: "MammalKernel",
            dependencies: []),
        .testTarget(
            name: "MammalKernelTests",
            dependencies: ["MammalKernel"]),
    ]
)
