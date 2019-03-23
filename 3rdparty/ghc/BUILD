load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_toolchain",
)

package(default_visibility = ["//visibility:public"])

haskell_toolchain(
    name = "haskell_toolchain",
    tools = "@raziel_ghc//:bin",
    version = "8.6.4",
)

base_deps = [
    "base",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "filepath",
    "hashable",
    "mtl",
    "process",
    "template-haskell",
    "text",
    "transformers",
]

nix_deps = [
    "exceptions",
    "fgl",
    "fgl-visualize",
    "generic-monoid",
    "lens-family",
    "lens-labels",
    "megaparsec",
    "optparse-applicative",
    "proto-lens",
    "raw-strings-qq",
    "split",
    "tasty",
    "tasty-hunit",
    "these",
    "unordered-containers",
    "xml-conduit",
]

[
    haskell_import(name = name)
    for name in base_deps + nix_deps
]
