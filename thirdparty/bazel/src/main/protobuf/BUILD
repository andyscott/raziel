package(default_visibility = ["//visibility:public"])

# https://github.com/tweag/rules_haskell/issues/717
strip_import_prefix = "/thirdparty/bazel/"

proto_library(
    name = "option_filters_proto",
    srcs = ["option_filters.proto"],
    strip_import_prefix = strip_import_prefix,
    visibility = ["//visibility:private"],
)

proto_library(
    name = "command_line_proto",
    srcs = ["command_line.proto"],
    strip_import_prefix = strip_import_prefix,
    deps = [":option_filters_proto"],
)

proto_library(
    name = "invocation_policy_proto",
    srcs = ["invocation_policy.proto"],
    strip_import_prefix = strip_import_prefix,
)
