load("//tools:shell.bzl", "shell_binary")

shell_binary(
    name = "ci",
    srcs = ["src/ci.sh"],
    tags = ["manual"],
    deps = [
        ":lint",
        "@bazel_tools//tools/bash/runfiles",
    ],
)

shell_binary(
    name = "lint",
    srcs = ["src/lint.sh"],
    tags = ["manual"],
    deps = [
        "@bazel_tools//tools/bash/runfiles",
    ],
)
