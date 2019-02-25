load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_proto_library",
)

package(default_visibility = ["//visibility:public"])

proto_library(
    name = "build_event_stream_proto",
    srcs = ["build_event_stream.proto"],
    #strip_import_prefix = "/thirdparty/bazel/",
    deps = [
        "//thirdparty/bazel/src/main/protobuf:command_line_proto",
        "//thirdparty/bazel/src/main/protobuf:invocation_policy_proto",
    ],
)

haskell_proto_library(
    name = "build_event_stream_haskell_proto",
    deps = [":build_event_stream_proto"],
)
