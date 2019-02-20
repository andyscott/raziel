load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_toolchain",
)

package(default_visibility = ["//visibility:public"])

haskell_toolchain(
    name = "haskell_toolchain",
    tools = "@raziel_ghc//:bin",
    version = "8.6.3",
)

base_deps = [
    "base",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "filepath",
    "mtl",
    "process",
    "text",
    "transformers",
]

nix_deps = [
    "exceptions",
    "split",
    "proto-lens",
    "lens-family",
    "lens-labels",
    "tasty",
    "tasty-hunit",
    "xml-conduit",
    "raw-strings-qq",
    "attoparsec",
]

[
    haskell_import(name = name)
    for name in base_deps + nix_deps
]
