#!/usr/bin/env bash
set -euo pipefail

# Determine the base branch for change detection.
base="${BUILDKITE_PULL_REQUEST_BASE_BRANCH:-main}"

# On the main branch itself, diff against the parent commit so we
# still get targeted builds for each push.
if [ "${BUILDKITE_BRANCH:-}" = "main" ]; then
  changed=$(git diff --name-only HEAD~1...HEAD 2>/dev/null | jq -R . | jq -s . || echo '[]')
else
  git fetch origin "$base" --depth=50 2>/dev/null || true
  changed=$(git diff --name-only "origin/${base}...HEAD" | jq -R . | jq -s .)
fi

is_main="false"
if [ "${BUILDKITE_BRANCH:-}" = "main" ]; then
  is_main="true"
fi

echo "--- :mag: Changed files"
echo "$changed" | jq -r '.[]'

eval_pipeline() {
  local pipeline_file="$1"
  local extra_args="$2"

  nix eval --impure --json --expr "
    let lib = (builtins.getFlake \"nixpkgs\").lib;
        files = builtins.fromJSON ''${changed}'';
    in import ./${pipeline_file} ({
      inherit lib;
      changedFiles = files;
    } // ${extra_args})
  "
}

echo "--- :nix: Evaluating main pipeline"
eval_pipeline "nix/ci/pipelines/main.nix" "{ isMain = ${is_main}; }" > pipeline-main.json

echo "--- :nix: Evaluating interop pipeline"
eval_pipeline "nix/ci/pipelines/interop.nix" "{}" > pipeline-interop.json

echo "--- :pipeline: Uploading main pipeline"
buildkite-agent pipeline upload pipeline-main.json

echo "--- :pipeline: Uploading interop pipeline"
buildkite-agent pipeline upload pipeline-interop.json

echo "--- :white_check_mark: Pipeline generation complete"
echo "Main pipeline steps: $(jq '.steps | length' pipeline-main.json)"
echo "Interop pipeline steps: $(jq '.steps | length' pipeline-interop.json)"
