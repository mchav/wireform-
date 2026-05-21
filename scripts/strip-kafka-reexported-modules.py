#!/usr/bin/env python3
"""Remove Cabal reexported-modules from wireform-kafka.cabal (keeps Haddock lean)."""
from __future__ import annotations

import re
import sys
from pathlib import Path


def main() -> None:
    path = Path(sys.argv[1] if len(sys.argv) > 1 else "wireform-kafka/wireform-kafka.cabal")
    text = path.read_text()
    updated = re.sub(
        r"\n    reexported-modules:\n(?:        [^\n]+\n)+?(?=    build-depends:)",
        "\n",
        text,
        count=1,
    )
    if updated == text:
        print("no reexported-modules block found", file=sys.stderr)
        sys.exit(1)
    path.write_text(updated)
    print(f"stripped reexported-modules from {path}")


if __name__ == "__main__":
    main()
