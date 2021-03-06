#!/usr/bin/env bash

# --- begin bash runfile prelude --
if [[ -e "${TEST_SRCDIR:-}" ]]; then
    function runfile() {
        echo "$TEST_SRCDIR/$1"
    }
elif [[ -f "$0.runfiles_manifest" ]]; then
    __runfiles_manifest_file="$0.runfiles_manifest"
    export __runfiles_manifest_file
    function runfile() {
        grep -m1 "^$1 " "$__runfiles_manifest_file" | cut -d ' ' -f 2-
    }
else
    echo "please run this script through bazel"
    exit 1
fi
export -f runfile
# --- end bash runfile prelude --

set -euo pipefail
cd "${BUILD_WORKSPACE_DIRECTORY:-$(dirname "$0")/..}"

bazel=./tools/bazel

$bazel query "$(cat <<-'EOF'
  let universe = //... - //src/Bazel/BuildEventStream/...
  in
    kind(shellcheck_test, $universe) +
    kind(haskell_lint, $universe)
EOF
)" | xargs $bazel test
