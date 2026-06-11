#!/usr/bin/env bash
#
# fourmolu wrapper that skips any Haskell source enabling the C
# preprocessor. fourmolu has no notion of CPP macro-call syntax and will
# happily reformat `WRAP_F(Int,intHost)` into `WRAP_F (Int, intHost)`,
# which silently disables the macro (GHC only treats a name as a
# function-like macro invocation when `(` immediately follows). Running
# the formatter on such files produces a parse error at build time, so we
# exclude them up front.
#
# Usage: fourmolu-no-cpp.sh <mode> [path ...]
#   <mode>   passed through as `fourmolu --mode <mode>` (e.g. inplace, check)
#   [path]   files and/or directories. Directories are searched recursively
#            for Haskell sources. When invoked as a pre-commit `entry`, the
#            hook appends the staged file list here.
#
# Portable to bash 3.2 (macOS system bash): no `mapfile`/`readarray`.

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "usage: $0 <mode> [path ...]" >&2
  exit 2
fi

mode="$1"
shift

# A file enables CPP if a LANGUAGE pragma line mentions CPP as a whole word.
# `grep -w` is supported by both GNU and BSD grep, avoiding non-portable
# `\b` word-boundary escapes.
uses_cpp() {
  grep -E '\{-#[[:space:]]*LANGUAGE' "$1" 2>/dev/null | grep -qw CPP
}

keep=()
for arg in "$@"; do
  if [ -d "$arg" ]; then
    while IFS= read -r f; do
      if uses_cpp "$f"; then
        continue
      fi
      keep+=("$f")
    done < <(find "$arg" -type f \( -name '*.hs' -o -name '*.lhs' -o -name '*.hs-boot' \))
  elif [ -f "$arg" ]; then
    if uses_cpp "$arg"; then
      continue
    fi
    keep+=("$arg")
  fi
done

if [ "${#keep[@]}" -eq 0 ]; then
  exit 0
fi

exec fourmolu --mode "$mode" "${keep[@]}"
