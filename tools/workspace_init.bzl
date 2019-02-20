load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

def raziel_workspace_init():
    haskell_repositories()
    nixpkgs_local_repository(
        name = "raziel_nixpkgs",
        nix_file = "//3rdparty:nixpkgs.nix",
    )
    nixpkgs_package(
        name = "raziel_ghc",
        build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
        nix_file = "//3rdparty:ghc.nix",
        repository = "@raziel_nixpkgs",
    )
    native.register_toolchains("//3rdparty/ghc:haskell_toolchain")
    native.register_toolchains("//toolchains:haskell-proto-toolchain")
