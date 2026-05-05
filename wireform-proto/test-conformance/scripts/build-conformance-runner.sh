#!/usr/bin/env bash
# Build the upstream protobuf conformance_test_runner.
#
# This script clones https://github.com/protocolbuffers/protobuf
# into ./dist-newstyle/conformance/protobuf/ (workspace-relative)
# at a pinned tag, configures with cmake, and builds the
# conformance_test_runner binary. The binary lands at
#   ./dist-newstyle/conformance/conformance_test_runner
# which is exactly where the protobuf-conformance-test suite
# (Test.Conformance.Driver) looks by default.
#
# Re-running the script is a no-op once the binary exists. Pass
# --force to wipe the cached tree and rebuild.
#
# Requires: git, cmake (>= 3.13), a C++17 compiler, abseil
# (cmake will pull it in via FetchContent).

set -euo pipefail

PROTOBUF_TAG="${PROTOBUF_TAG:-v28.2}"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
CACHE_DIR="$ROOT/dist-newstyle/conformance"
PROTOBUF_DIR="$CACHE_DIR/protobuf"
BUILD_DIR="$PROTOBUF_DIR/build"
RUNNER="$CACHE_DIR/conformance_test_runner"

usage() {
  cat <<EOF
Usage: $0 [--force]

Builds the upstream protobuf conformance_test_runner under
$CACHE_DIR.

Environment:
  PROTOBUF_TAG   Git tag to check out (default: $PROTOBUF_TAG)
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if [[ "${1:-}" == "--force" ]]; then
  rm -rf "$CACHE_DIR"
fi

if [[ -x "$RUNNER" ]]; then
  echo "conformance_test_runner already built at $RUNNER"
  echo "(pass --force to rebuild)"
  exit 0
fi

mkdir -p "$CACHE_DIR"

if [[ ! -d "$PROTOBUF_DIR/.git" ]]; then
  echo ">> cloning protocolbuffers/protobuf @ $PROTOBUF_TAG"
  git clone --depth 1 --branch "$PROTOBUF_TAG" \
    https://github.com/protocolbuffers/protobuf.git "$PROTOBUF_DIR"
else
  echo ">> reusing existing checkout at $PROTOBUF_DIR"
fi

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# We only need the conformance_test_runner target. Build with
# -DCMAKE_BUILD_TYPE=Release for a usable binary; the upstream
# tree's Debug build is glacial.
echo ">> configuring (this fetches abseil etc. on first run; ~2 min)"
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -Dprotobuf_BUILD_TESTS=ON \
  -Dprotobuf_BUILD_CONFORMANCE=ON \
  -Dprotobuf_ABSL_PROVIDER=module

echo ">> building conformance_test_runner (this is the slow part; ~10 min)"
cmake --build . --target conformance_test_runner -j "$(nproc 2>/dev/null || echo 4)"

cp "$BUILD_DIR/conformance_test_runner" "$RUNNER"
chmod +x "$RUNNER"

echo
echo "Built: $RUNNER"
echo "Run the conformance suite with:"
echo "    cabal test wireform-proto:protobuf-conformance-test"
