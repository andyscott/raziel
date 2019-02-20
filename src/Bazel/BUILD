load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "Workspace",
    srcs = ["Workspace.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:directory",
        "//3rdparty/ghc:exceptions",
        "//3rdparty/ghc:filepath",
        "//3rdparty/ghc:split",
        "//3rdparty/ghc:transformers",
    ],
)

haskell_library(
    name = "LabelNode",
    srcs = ["LabelNode.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:attoparsec",
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:text",
    ],
)

haskell_test(
    name = "LabelNodeSpec",
    srcs = ["LabelNodeSpec.hs"],
    main_function = "Bazel.LabelNodeSpec.main",
    visibility = ["//visibility:public"],
    deps = [
        ":LabelNode",
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:tasty",
        "//3rdparty/ghc:tasty-hunit",
        "//3rdparty/ghc:text",
    ],
)
