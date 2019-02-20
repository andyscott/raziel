load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",    
    "haskell_proto_toolchain",    
    "haskell_toolchain",
)

haskell_proto_toolchain(
    name = "haskell-proto-toolchain",
    plugin = "@proto-lens-protoc//:bin/proto-lens-protoc",
    protoc = "@com_google_protobuf//:protoc",    
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:bytestring",
        "//3rdparty/ghc:containers",
        "//3rdparty/ghc:deepseq",
        "//3rdparty/ghc:lens-family",
        "//3rdparty/ghc:lens-labels",
        "//3rdparty/ghc:mtl",
        "//3rdparty/ghc:proto-lens",
        "//3rdparty/ghc:text",
    ],
)
