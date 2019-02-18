load("//tools:shellcheck.bzl", "shellcheck_test")

sh_binary(
    name = "ci",
    srcs = ["src/ci.sh"],
    tags = ["manual"],
    deps = [
        ":lint",
        "@bazel_tools//tools/bash/runfiles",
    ],
)

sh_binary(
    name = "lint",
    srcs = ["src/lint.sh"],
    tags = ["manual"],
    deps = [
        "@bazel_tools//tools/bash/runfiles",
    ],
)

shellcheck_test(
    name = "src@shellcheck",
    src = glob(["src/*.sh"]),
)
