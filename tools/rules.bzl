load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    upstream_haskell_binary = "haskell_binary",
    upstream_haskell_library = "haskell_library",
    upstream_haskell_lint = "haskell_lint",
    upstream_haskell_test = "haskell_test",
)

def haskell_library(**kwargs):
    upstream_haskell_library(**kwargs)
    upstream_haskell_lint(
        name = "%s@lint" % kwargs["name"],
        tags = ["manual"],
        deps = [
            ":%s" % kwargs["name"],
        ],
    )

def haskell_binary(**kwargs):
    upstream_haskell_binary(**kwargs)
    upstream_haskell_lint(
        name = "%s@lint" % kwargs["name"],
        tags = ["manual"],
        deps = [
            ":%s" % kwargs["name"],
        ],
    )

def haskell_test(**kwargs):
    upstream_haskell_test(**kwargs)
    upstream_haskell_lint(
        name = "%s@lint" % kwargs["name"],
        tags = ["manual"],
        deps = [
            ":%s" % kwargs["name"],
        ],
    )
