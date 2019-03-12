load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_binary(
    name = "Graph",
    srcs = ["Graph.hs"],
    main_function = "Bazel.Graph.main",
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:fgl",
        "//3rdparty/ghc:fgl-visualize",
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
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:megaparsec",
        "//3rdparty/ghc:text",
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
