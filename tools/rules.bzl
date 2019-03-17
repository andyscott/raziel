load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    upstream_haskell_binary = "haskell_binary",
    upstream_haskell_library = "haskell_library",
    upstream_haskell_lint = "haskell_lint",
    upstream_haskell_test = "haskell_test",
)

def haskell_library(**kwargs):
    (rule_args, lint_args) = bifurcate_kwargs(kwargs)
    print(rule_args)
    upstream_haskell_library(**rule_args)
    if lint_args != None:
        upstream_haskell_lint(**lint_args)

def haskell_binary(**kwargs):
    (rule_args, lint_args) = bifurcate_kwargs(kwargs)
    upstream_haskell_binary(**rule_args)
    if lint_args != None:
        upstream_haskell_lint(**lint_args)

def haskell_test(**kwargs):
    (rule_args, lint_args) = bifurcate_kwargs(kwargs)
    upstream_haskell_test(**rule_args)
    if lint_args != None:
        lint_args["testonly"] = True
        upstream_haskell_lint(**lint_args)

def bifurcate_kwargs(kwargs):
    rule_args = dict(kwargs)
    lint_args = None

    if "lint" in kwargs:
        rule_args.pop("lint")
    if kwargs.get("lint", True):
        lint_args = {
            "name": "%s@lint" % rule_args["name"],
            "testonly": True,
            "tags": ["manual"],
            "deps": [
                ":%s" % rule_args["name"],
            ],
        }

    return (rule_args, lint_args)
