load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "XML",
    srcs = ["XML.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:containers",
        "//3rdparty/ghc:exceptions",
        "//3rdparty/ghc:generic-monoid",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:xml-conduit",
        "//src/Bazel:Label",
    ],
)

haskell_test(
    name = "XMLSpec",
    srcs = ["XMLSpec.hs"],
    main_function = "Bazel.Query.XMLSpec.main",
    visibility = ["//visibility:public"],
    deps = [
        ":XML",
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:raw-strings-qq",
        "//3rdparty/ghc:tasty",
        "//3rdparty/ghc:tasty-hunit",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:xml-conduit",
    ],
)
