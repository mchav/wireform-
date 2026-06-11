#!/usr/bin/env bash
# Run the Autobahn|Testsuite WebSocket conformance suite against the
# wireform-websocket echo server.
#
# Usage:
#   wireform-websocket/scripts/run-autobahn.sh [--quick]
#
# Requirements:
#   * docker (or podman with `docker` alias) with the
#     crossbario/autobahn-testsuite image accessible
#   * cabal + GHC able to build the wireform-websocket-autobahn-echo
#     executable
#
# Output:
#   test-conformance/reports/servers/index.json  — machine-readable
#   test-conformance/reports/servers/*.html      — per-case detail
#
# Exit status is 0 iff every non-excluded case reports OK or
# NON-STRICT (informational).  Cases excluded in
# config/fuzzingclient.json (9.* perf, 12.*/13.* permessage-deflate)
# are not run; see the spec file for the canonical list.

set -euo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
cd "$HERE"

QUICK=0
if [[ "${1:-}" == "--quick" ]]; then
  QUICK=1
fi

# 1. Build the echo server.
echo "==> Building wireform-websocket-autobahn-echo …"
cabal build --project-dir "$HERE/.." wireform-websocket:exe:wireform-websocket-autobahn-echo

ECHO_BIN="$(cabal list-bin --project-dir "$HERE/.." wireform-websocket:exe:wireform-websocket-autobahn-echo)"

# 2. Start the echo server in the background.
# Ensure the report dir exists: both the server-log redirect below and
# the docker `-v …/reports:/reports` mount need it present up front
# (otherwise docker creates it root-owned and the redirect fails first).
mkdir -p "$HERE/test-conformance/reports"
PORT=9001
echo "==> Starting echo server on 127.0.0.1:$PORT …"
WIREFORM_AUTOBAHN_PORT="$PORT" "$ECHO_BIN" >"$HERE/test-conformance/reports/echo-server.log" 2>&1 &
ECHO_PID=$!
trap 'kill "$ECHO_PID" 2>/dev/null || true' EXIT

# Wait until the listener is up.
for i in $(seq 1 50); do
  if (echo > /dev/tcp/127.0.0.1/"$PORT") 2>/dev/null; then
    break
  fi
  sleep 0.1
done

# 3. Pick a fuzzingclient spec.
SPEC="config/fuzzingclient.json"
if [[ "$QUICK" -eq 1 ]]; then
  SPEC="config/fuzzingclient-quick.json"
fi

# 4. Drive the suite.  --network host so the container can reach the
#    echo server on 127.0.0.1.  On macOS that's `host.docker.internal`
#    but on Linux --network host works directly; the spec uses
#    host.docker.internal so we add it as an alias when not on Linux.
DOCKER_NET=(--network host)
case "$(uname -s)" in
  Darwin) DOCKER_NET=(--add-host=host.docker.internal:host-gateway) ;;
esac

echo "==> Running Autobahn fuzzingclient against echo server …"
${DOCKER:-docker} run --rm \
  "${DOCKER_NET[@]}" \
  -v "$HERE/test-conformance/config:/config" \
  -v "$HERE/test-conformance/reports:/reports" \
  crossbario/autobahn-testsuite \
  wstest -m fuzzingclient -s "/config/$(basename "$SPEC")"

# 5. Summarise the JSON report.
echo "==> Parsing report …"
python3 "$HERE/scripts/autobahn-summary.py" \
  "$HERE/test-conformance/reports/servers/index.json"
