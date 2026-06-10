#!/usr/bin/env bash
# Bootstrap GHC/Cabal on a fresh Ubuntu VM per AGENTS.md.
set -euo pipefail

if ! command -v apt-get >/dev/null 2>&1; then
  echo "This script targets Debian/Ubuntu (apt-get)." >&2
  exit 1
fi

if [[ "$(id -u)" -eq 0 ]]; then
  apt-get update -qq
  apt-get install -y --no-install-recommends \
    build-essential curl ca-certificates \
    libgmp-dev libffi-dev libncurses-dev libtinfo6 \
    zlib1g-dev libnuma-dev xz-utils \
    pkg-config protobuf-compiler \
    libsnappy-dev liblz4-dev libzstd-dev
  TARGET_USER="${SUDO_USER:-ubuntu}"
else
  TARGET_USER="${USER}"
fi

if ! command -v ghcup >/dev/null 2>&1; then
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
      BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
      BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
      sh
fi

GHCUP_DIR="$(eval echo "~${TARGET_USER}")/.ghcup"
# shellcheck source=/dev/null
source "${GHCUP_DIR}/env"

ghcup install ghc 9.8.4 --set
ghcup install ghc 9.6.4
ghcup install cabal 3.10.3.0 --set || ghcup install cabal latest --set

cabal update

echo "GHC:  $(ghc --version)"
echo "Cabal: $(cabal --version)"
echo "Source ${GHCUP_DIR}/env before building."
