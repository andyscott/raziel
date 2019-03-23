load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "LabelGraph",
    srcs = ["LabelGraph.hs"],
    #main_function = "Bazel.LabelGraph.main",
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:containers",
        "//3rdparty/ghc:exceptions",
        "//3rdparty/ghc:fgl",
        "//3rdparty/ghc:mtl",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:unordered-containers",
        "//src/Bazel:Label",
        "//src/Bazel/Query:XML",
    ],
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
    name = "Label",
    srcs = ["Label.hs"],
    lint = False,
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:exceptions",
        "//3rdparty/ghc:hashable",
        "//3rdparty/ghc:megaparsec",
        "//3rdparty/ghc:template-haskell",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:these",
    ],
)

haskell_test(
    name = "LabelSpec",
    srcs = ["LabelSpec.hs"],
    main_function = "Bazel.LabelSpec.main",
    visibility = ["//visibility:public"],
    deps = [
        ":Label",
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:tasty",
        "//3rdparty/ghc:tasty-hunit",
        "//3rdparty/ghc:text",
    ],
)
