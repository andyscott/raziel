load(
    "//tools:rules.bzl",
    "haskell_library",
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
