load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

default_name = package_name().split("/")[-1]

alias(
    name = default_name,
    actual = ":Main",
)

haskell_binary(
    name = "Main",
    srcs = glob(["*.hs"]),
    main_function = "Raziel.Main.main",
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//3rdparty/ghc:directory",
        "//3rdparty/ghc:fgl",
        "//3rdparty/ghc:filepath",
        "//3rdparty/ghc:mtl",
        "//3rdparty/ghc:optparse-applicative",
        "//3rdparty/ghc:process",
        "//3rdparty/ghc:text",
        "//3rdparty/ghc:these",
        "//src/Bazel:Label",
        "//src/Bazel:LabelGraph",
        "//src/Bazel:Workspace",
        "//src/Bazel/Query:XML",
    ],
)
