#!/usr/bin/env python3
"""Force protocol modules to resolve via wireform-kafka-protocol (PackageImports)."""
from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PKG = "wireform-kafka-protocol"

MAIN_PROTOCOL_MODULES = frozenset({
    "Kafka.Protocol.ApiVersions",
    "Kafka.Protocol.VersionNegotiation",
    "Kafka.Protocol.CRC32C",
    "Kafka.Protocol.RecordBatch",
    "Kafka.Protocol.RecordBatchWire",
})

PROTOCOL_MODULE_PREFIXES = (
    "Kafka.Protocol.Generated.",
    "Kafka.Protocol.Wire.",
    "Kafka.Protocol.Wire",
    "Kafka.Protocol.Primitives",
    "Kafka.Protocol.Message",
)


def module_name(path: Path) -> str | None:
    text = path.read_text(encoding="utf-8")
    m = re.search(r"^module\s+([\w.]+)", text, re.M)
    return m.group(1) if m else None


def is_protocol_only_module(mod: str) -> bool:
    if mod in MAIN_PROTOCOL_MODULES:
        return False
    if mod.startswith("Kafka.Protocol.Generated."):
        return True
    return mod in {
        "Kafka.Protocol.Primitives",
        "Kafka.Protocol.Message",
        "Kafka.Protocol.Wire",
        "Kafka.Protocol.Wire.Codec",
        "Kafka.Protocol.Wire.Primitives",
        "Kafka.Protocol.Wire.SliceVector",
    }


def needs_package_import(line: str) -> bool:
    if "import" not in line or "import " not in line:
        return False
    if f'"{PKG}"' in line:
        return False
    stripped = line.strip()
    if not stripped.startswith("import"):
        return False
    for prefix in PROTOCOL_MODULE_PREFIXES:
        if prefix in stripped:
            return True
    return False


def add_package_import(line: str) -> str:
    if f'"{PKG}"' in line:
        return line
    # import qualified M as A  /  import M (x)  /  import M
    return re.sub(
        r"^(\s*import\s+(?:qualified\s+)?)",
        rf'\1"{PKG}" ',
        line,
        count=1,
    )


def ensure_language_pragma(text: str) -> str:
    if "PackageImports" in text:
        return text
    if "{-# LANGUAGE" in text.split("\n", 1)[0] or text.startswith("{-# LANGUAGE"):
        # append to first LANGUAGE pragma block
        return re.sub(
            r"(\{-# LANGUAGE[^#]*#\})",
            r"\1\n{-# LANGUAGE PackageImports #-}",
            text,
            count=1,
        )
    return "{-# LANGUAGE PackageImports #-}\n\n" + text


def patch_file(path: Path) -> bool:
    mod = module_name(path)
    if mod and is_protocol_only_module(mod):
        return False
    original = path.read_text(encoding="utf-8")
    lines = original.splitlines(keepends=True)
    changed = False
    out: list[str] = []
    for line in lines:
        if needs_package_import(line):
            new_line = add_package_import(line)
            if new_line != line:
                changed = True
            out.append(new_line)
        else:
            out.append(line)
    if not changed:
        return False
    text = "".join(out)
    text = ensure_language_pragma(text)
    path.write_text(text, encoding="utf-8")
    return True


def main() -> None:
    roots = [
        ROOT / "wireform-kafka" / "src",
        ROOT / "wireform-kafka" / "test",
        ROOT / "wireform-kafka" / "test-conformance",
        ROOT / "wireform-kafka" / "test-integration",
        ROOT / "wireform-kafka" / "bench",
        ROOT / "wireform-kafka" / "streams" / "src",
        ROOT / "wireform-kafka" / "streams" / "test",
        ROOT / "wireform-kafka" / "examples",
        ROOT / "wireform-kafka" / "codegen-exe",
        ROOT / "wireform-kafka" / "codegen-emit-snapshot",
        ROOT / "wireform-kafka" / "test-data" / "gen-test-vectors",
        ROOT / "wireform-kafka" / "bench" / "perf-tool",
    ]
    n = 0
    for root in roots:
        if not root.is_dir():
            continue
        for path in sorted(root.rglob("*.hs")):
            if patch_file(path):
                n += 1
                print(path.relative_to(ROOT))
    print(f"patched {n} files")


if __name__ == "__main__":
    main()
