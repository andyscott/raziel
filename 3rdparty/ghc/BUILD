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
