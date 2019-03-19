load(
    "//tools:rules.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    name = "BuildEventStream",
    srcs = ["BuildEventStream.hs"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/ghc:base",
        "//thirdparty/bazel/src/main/java/com/google/devtools/build/lib/buildeventstream/proto:build_event_stream_haskell_proto",
    ],
)
