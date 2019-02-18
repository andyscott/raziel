workspace(name = "archangel_raziel")

# If you're reading this, and you want to use Raziel
# from within another Bazel workspace, you can hopefully just copy...

# BEGIN portable bits

load("@archangel_raziel//tools:workspace_prelude.bzl", "raziel_workspace_prelude")

raziel_workspace_prelude()

load("@archangel_raziel//tools:workspace_init.bzl", "raziel_workspace_init")

raziel_workspace_init()

# END portable bits

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
)

nixpkgs_package(
    name = "shellcheck",
    repository = "@raziel_nixpkgs",
)
