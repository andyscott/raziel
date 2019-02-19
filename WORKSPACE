workspace(name = "archangel_raziel")

# If you're reading this, and you want to use Raziel
# from within another Bazel workspace, you can hopefully just copy...

# BEGIN portable bits

load("@archangel_raziel//tools:workspace_prelude.bzl", "raziel_workspace_prelude")

raziel_workspace_prelude()

load("@archangel_raziel//tools:workspace_init.bzl", "raziel_workspace_init")

raziel_workspace_init()

# END portable bits

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "rules_adroit",
    commit = "22d2d8dfcad3eadbad07281f0db2e90d5a7e5e65",
    remote = "git://github.com/andyscott/rules_adroit",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
)

nixpkgs_package(
    name = "shellcheck",
    repository = "@raziel_nixpkgs",
)

register_toolchains(
    "@rules_adroit//toolchains:shellcheck_from_nixpkgs",
)
