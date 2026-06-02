#!/usr/bin/env bash
#
# Cross-language gRPC interop test runner.
#
# Uses the official gRPC Docker interop infrastructure to test wireform-grpc
# against reference implementations (Python, C++, Go, Java).
#
# Prerequisites:
#   - Docker (running)
#   - A checkout of the gRPC repo (see below)
#   - Python 3 with PyYAML (for run_interop_tests.py)
#
# gRPC repo setup (do NOT use --depth 1, submodules need full history):
#
#   git clone https://github.com/grpc/grpc.git -b v1.72.2 /path/to/grpc-repo
#   cd /path/to/grpc-repo
#   git switch -c v1.72.2
#   git submodule update --init --recursive
#
# For Go and Java, clone the separate repos alongside grpc-repo:
#
#   git clone https://github.com/grpc/grpc-go.git -b v1.72.2 /path/to/grpc-go
#   git clone https://github.com/grpc/grpc-java.git -b v1.73.0 /path/to/grpc-java
#
# Usage:
#   ./wireform-grpc/scripts/cross-language-interop.sh [OPTIONS]
#
# Options:
#   --grpc-repo PATH    Path to grpc repo checkout (default: $GRPC_REPO or ../grpc-repo)
#   --languages LANGS   Comma-separated list of languages to test (default: python,cxx)
#                        Available: python, cxx, go, java
#   --client-only        Only test wireform as client (against reference servers)
#   --server-only        Only test wireform as server (against reference clients)
#   --generate-only      Only generate Docker scripts, don't run tests
#   --skip-build         Skip building wireform-grpc-interop binary
#   --help               Show this help
#
# Per-language skip flags (from upstream interop test definitions):
#   Python: --skip_compression
#   Go:     --skip_compression
#   Java:   --skip_client_compression
#           --skip_test=server_compressed_streaming
#           --skip_test=timeout_on_sleeping_server
#   C++:    (no skips needed)
#
# See also: /tmp/grapesy/dev/interop.md
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

WIREFORM_SERVER_PORT=50052
PASSED=0
FAILED=0
SKIPPED=0
TOTAL=0
PIDS_TO_KILL=()

# Defaults
GRPC_REPO="${GRPC_REPO:-}"
LANGUAGES="python,cxx"
RUN_CLIENT=true
RUN_SERVER=true
GENERATE_ONLY=false
SKIP_BUILD=false

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
NC='\033[0m'

usage() {
  head -n 47 "$0" | tail -n +2 | sed 's/^# \?//'
  exit 0
}

cleanup() {
  for pid in "${PIDS_TO_KILL[@]+"${PIDS_TO_KILL[@]}"}"; do
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  done
  if command -v docker &>/dev/null; then
    # Stop any interop containers we started
    docker ps -q --filter "name=grpc_interop_" 2>/dev/null | while read -r cid; do
      docker stop "$cid" 2>/dev/null || true
    done
  fi
}
trap cleanup EXIT

log()  { echo -e "${BOLD}[interop]${NC} $*"; }
pass() { echo -e "  ${GREEN}PASS${NC} $1"; PASSED=$((PASSED + 1)); TOTAL=$((TOTAL + 1)); }
fail() { echo -e "  ${RED}FAIL${NC} $1${2:+: $2}"; FAILED=$((FAILED + 1)); TOTAL=$((TOTAL + 1)); }
skip() { echo -e "  ${YELLOW}SKIP${NC} $1${2:+: $2}"; SKIPPED=$((SKIPPED + 1)); }

# ─── Argument parsing ───────────────────────────────────────────────────────

while [[ $# -gt 0 ]]; do
  case "$1" in
    --grpc-repo)   GRPC_REPO="$2";   shift 2 ;;
    --languages)   LANGUAGES="$2";   shift 2 ;;
    --client-only) RUN_SERVER=false;  shift ;;
    --server-only) RUN_CLIENT=false;  shift ;;
    --generate-only) GENERATE_ONLY=true; shift ;;
    --skip-build)  SKIP_BUILD=true;   shift ;;
    --help|-h)     usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

IFS=',' read -ra LANG_ARRAY <<< "$LANGUAGES"

# ─── Prerequisite checks ────────────────────────────────────────────────────

check_prerequisites() {
  local ok=true

  if ! command -v docker &>/dev/null; then
    echo "ERROR: Docker is not installed."
    echo "  Install Docker: https://docs.docker.com/get-docker/"
    ok=false
  elif ! docker info &>/dev/null 2>&1; then
    echo "ERROR: Docker daemon is not running."
    echo "  Start Docker and try again."
    ok=false
  fi

  if [ -z "$GRPC_REPO" ]; then
    # Try common locations
    for candidate in \
      "$REPO_ROOT/../grpc-repo" \
      "$REPO_ROOT/../grpc" \
      "$HOME/grpc-repo" \
      "/tmp/grpc-repo"; do
      if [ -d "$candidate/tools/run_tests" ]; then
        GRPC_REPO="$(cd "$candidate" && pwd)"
        break
      fi
    done
  fi

  if [ -z "$GRPC_REPO" ] || [ ! -d "$GRPC_REPO/tools/run_tests" ]; then
    echo "ERROR: gRPC repo not found."
    echo ""
    echo "  Clone the official gRPC repo (do NOT use --depth 1):"
    echo ""
    echo "    git clone https://github.com/grpc/grpc.git -b v1.72.2 /path/to/grpc-repo"
    echo "    cd /path/to/grpc-repo"
    echo "    git switch -c v1.72.2"
    echo "    git submodule update --init --recursive"
    echo ""
    echo "  Then either:"
    echo "    export GRPC_REPO=/path/to/grpc-repo"
    echo "    # or"
    echo "    $0 --grpc-repo /path/to/grpc-repo"
    echo ""
    echo "  For Go/Java interop, also clone alongside grpc-repo:"
    echo "    git clone https://github.com/grpc/grpc-go.git -b v1.72.2 /path/to/grpc-go"
    echo "    git clone https://github.com/grpc/grpc-java.git -b v1.73.0 /path/to/grpc-java"
    ok=false
  fi

  if ! "$ok"; then
    exit 1
  fi

  if [ ! -f "$GRPC_REPO/tools/run_tests/run_interop_tests.py" ]; then
    echo "ERROR: $GRPC_REPO does not contain tools/run_tests/run_interop_tests.py"
    echo "  Is this a valid gRPC repo checkout?"
    exit 1
  fi

  log "gRPC repo: $GRPC_REPO"
}

# ─── Per-language skip flags ────────────────────────────────────────────────
#
# These match upstream's unimplemented_test_cases_server definitions in
# run_interop_tests.py (class PythonLanguage, etc.) plus known server
# non-conformance (Java timeout_on_sleeping_server).

wireform_client_skip_flags() {
  local lang=$1
  case "$lang" in
    python) echo "--skip_compression" ;;
    cxx)    echo "" ;;
    go)     echo "--skip_compression" ;;
    java)   echo "--skip_client_compression --skip_test=server_compressed_streaming --skip_test=timeout_on_sleeping_server" ;;
    *)      echo "" ;;
  esac
}

# ─── Generate Docker scripts ────────────────────────────────────────────────

generate_docker_scripts() {
  local lang=$1
  local script_lang

  # run_interop_tests.py uses "c++" not "cxx"
  case "$lang" in
    cxx) script_lang="c++" ;;
    *)   script_lang="$lang" ;;
  esac

  log "Generating Docker scripts for $lang..."

  (
    cd "$GRPC_REPO"
    python3 tools/run_tests/run_interop_tests.py \
      -l "$script_lang" -s "$script_lang" \
      --use_docker --manual 2>&1 | tail -5

    mv interop_server_cmds.sh "${lang}_server.sh"
    mv interop_client_cmds.sh "${lang}_client.sh"
  )

  log "Generated ${lang}_server.sh and ${lang}_client.sh"
}

# ─── Wait for a Docker container's mapped port ──────────────────────────────

get_docker_port() {
  local image_prefix=$1
  local retries=30
  local i=0

  while [ "$i" -lt "$retries" ]; do
    local port
    port=$(docker ps --format '{{.Image}} {{.Ports}}' 2>/dev/null \
      | grep "$image_prefix" \
      | head -1 \
      | grep -oE '0\.0\.0\.0:[0-9]+' \
      | head -1 \
      | cut -d: -f2) || true

    if [ -n "${port:-}" ]; then
      echo "$port"
      return 0
    fi

    i=$((i + 1))
    sleep 1
  done

  return 1
}

wait_for_port() {
  local port=$1 timeout=${2:-30} i=0
  while ! nc -z 127.0.0.1 "$port" 2>/dev/null; do
    i=$((i + 1))
    if [ "$i" -ge "$timeout" ]; then
      return 1
    fi
    sleep 1
  done
}

# ─── Build wireform ─────────────────────────────────────────────────────────

WIREFORM_BIN=""

build_wireform() {
  if [ "$SKIP_BUILD" = true ] && [ -n "${WIREFORM_BIN:-}" ] && [ -x "$WIREFORM_BIN" ]; then
    log "Using pre-built binary: $WIREFORM_BIN"
    return
  fi

  log "Building wireform-grpc interop test binary..."
  (cd "$REPO_ROOT" && cabal build wireform-grpc:test:wireform-grpc-interop 2>&1 | tail -5)
  WIREFORM_BIN=$(cd "$REPO_ROOT" && cabal list-bin wireform-grpc:test:wireform-grpc-interop 2>/dev/null)

  if [ ! -x "$WIREFORM_BIN" ]; then
    echo "ERROR: Could not find wireform-grpc-interop binary"
    exit 1
  fi

  log "Binary: $WIREFORM_BIN"
}

# ─── Phase 1: wireform client ← reference servers (Docker) ──────────────────

run_wireform_as_client() {
  local lang=$1

  log "Starting $lang reference server (Docker)..."
  (cd "$GRPC_REPO" && bash "./${lang}_server.sh") &
  PIDS_TO_KILL+=($!)

  local port
  if ! port=$(get_docker_port "grpc_interop_${lang}"); then
    fail "$lang server" "Could not determine Docker-mapped port after 30s"
    return
  fi

  log "$lang server ready on port $port"

  local skip_flags
  skip_flags=$(wireform_client_skip_flags "$lang")

  log "Running wireform client against $lang server (port $port)..."
  # shellcheck disable=SC2086
  if "$WIREFORM_BIN" --client \
      --server_host=127.0.0.1 \
      --server_port="$port" \
      --use_tls false \
      $skip_flags; then
    pass "wireform client ← $lang server"
  else
    fail "wireform client ← $lang server" "wireform client exited non-zero"
  fi

  # Stop the reference server container
  docker ps -q --filter "ancestor=$(docker ps --format '{{.Image}}' | grep "grpc_interop_${lang}" | head -1)" 2>/dev/null \
    | while read -r cid; do docker stop "$cid" 2>/dev/null || true; done
}

# ─── Phase 2: wireform server ← reference clients (Docker) ──────────────────

run_wireform_as_server() {
  local lang=$1

  log "Running $lang reference client (Docker) against wireform server (port $WIREFORM_SERVER_PORT)..."

  if (cd "$GRPC_REPO" && SERVER_PORT="$WIREFORM_SERVER_PORT" bash "./${lang}_client.sh") 2>&1; then
    pass "wireform server ← $lang client"
  else
    fail "wireform server ← $lang client" "reference client exited non-zero"
  fi
}

# ─── Main ───────────────────────────────────────────────────────────────────

check_prerequisites

log "Cross-language gRPC interop tests"
log "================================="
log "Languages: ${LANGUAGES}"
log "gRPC repo: ${GRPC_REPO}"
log ""

# Generate Docker scripts for each language
for lang in "${LANG_ARRAY[@]}"; do
  generate_docker_scripts "$lang"
done

if [ "$GENERATE_ONLY" = true ]; then
  log "Docker scripts generated. Exiting (--generate-only)."
  exit 0
fi

build_wireform

# Phase 1: wireform as client against reference servers
if [ "$RUN_CLIENT" = true ]; then
  log ""
  log "═══════════════════════════════════════════════════════════════"
  log "  Phase 1: wireform client against reference servers (Docker)"
  log "═══════════════════════════════════════════════════════════════"

  for lang in "${LANG_ARRAY[@]}"; do
    log ""
    log "── $lang ──"
    run_wireform_as_client "$lang"
  done
fi

# Phase 2: wireform as server against reference clients
if [ "$RUN_SERVER" = true ]; then
  log ""
  log "═══════════════════════════════════════════════════════════════"
  log "  Phase 2: reference clients (Docker) against wireform server"
  log "═══════════════════════════════════════════════════════════════"

  log "Starting wireform server on port $WIREFORM_SERVER_PORT..."
  "$WIREFORM_BIN" --server --use_tls false --port "$WIREFORM_SERVER_PORT" &
  PIDS_TO_KILL+=($!)

  if ! wait_for_port "$WIREFORM_SERVER_PORT" 15; then
    echo "ERROR: wireform server did not start within 15s"
    exit 1
  fi
  log "wireform server ready on port $WIREFORM_SERVER_PORT"

  for lang in "${LANG_ARRAY[@]}"; do
    log ""
    log "── $lang ──"
    run_wireform_as_server "$lang"
  done

  # Stop wireform server
  kill "${PIDS_TO_KILL[-1]}" 2>/dev/null || true
  wait "${PIDS_TO_KILL[-1]}" 2>/dev/null || true
fi

# Results
log ""
log "═══ Results ═══"
log "Total: $TOTAL  Passed: $PASSED  Failed: $FAILED  Skipped: $SKIPPED"
if [ "$FAILED" -gt 0 ]; then
  exit 1
fi
exit 0

# NOTE: The official gRPC interop Docker images (grpc_interop_python,
# grpc_interop_cxx, etc.) are linux/amd64 only. They will NOT work on
# Apple Silicon Macs even with Docker's Rosetta emulation.
#
# To run cross-language interop:
# 1. Use an x86 Linux machine or CI runner
# 2. Or: clone grpc/grpc with full submodules and use run_interop_tests.py
#    (see wireform-grpc/../dev/interop.md pattern from grapesy)
#
# Self-test (wireform against itself) validates all 20 test cases and
# runs on any platform:
#   cabal run wireform-grpc:test:wireform-grpc-interop -- --self-test
