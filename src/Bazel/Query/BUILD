load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "Output",
    srcs = ["Output.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:containers",
        "//3rdparty/ghc:exceptions",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:xml-conduit",
    ],
)

haskell_test(
    name = "OutputSpec",
    srcs = ["OutputSpec.hs"],
    main_function = "Bazel.Query.OutputSpec.main",
    visibility = ["//visibility:public"],
    deps = [
        ":Output",
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:raw-strings-qq",
        "//3rdparty/ghc:tasty",
        "//3rdparty/ghc:tasty-hunit",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:xml-conduit",
    ],
)
