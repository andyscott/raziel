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
        "//3rdparty/ghc:optparse-applicative",
        "//3rdparty/ghc:text",
        "//src/Bazel:Label",
        "//src/Bazel/Query:XML",
    ],
)
