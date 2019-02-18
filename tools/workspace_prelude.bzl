load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

def raziel_workspace_prelude():
    if native.existing_rule("io_tweag_rules_nixpkgs") == None:
        git_repository(
            name = "io_tweag_rules_nixpkgs",
            commit = "40b5a9f23abca57f364c93245c7451206ef1a855",
            remote = "git://github.com/tweag/rules_nixpkgs",
        )

    if native.existing_rule("io_tweag_rules_haskell") == None:
        git_repository(
            name = "io_tweag_rules_haskell",
            commit = "a2a469c1d590623ba9fb072574f9059b25eadd43",
            remote = "git://github.com/tweag/rules_haskell",
        )
