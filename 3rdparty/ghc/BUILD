load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_toolchain",
)

package(default_visibility = ["//visibility:public"])

haskell_toolchain(
    name = "haskell_toolchain",
    tools = "@raziel_ghc//:bin",
    version = "8.4.3",
)

base_deps = [
    "base",
    "directory",
    "filepath",
    "mtl",
    "process",
    "transformers",
]

nix_deps = [
    "exceptions",
    "split",
]

[
    haskell_import(name = name)
    for name in base_deps + nix_deps
]
