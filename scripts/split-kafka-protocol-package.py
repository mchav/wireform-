#!/usr/bin/env python3
"""Split Kafka wire modules into wireform-kafka-protocol package (separate .cabal)."""
from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
KAFKA_CABAL = ROOT / "wireform-kafka" / "wireform-kafka.cabal"
PROTO_DIR = ROOT / "wireform-kafka-protocol"
PROTO_CABAL = PROTO_DIR / "wireform-kafka-protocol.cabal"
CABAL_PROJECT = ROOT / "cabal.project"

MAIN_PROTOCOL_MODULES = frozenset({
    "Kafka.Protocol.ApiVersions",
    "Kafka.Protocol.VersionNegotiation",
    "Kafka.Protocol.CRC32C",
    "Kafka.Protocol.RecordBatch",
    "Kafka.Protocol.RecordBatchWire",
})


def is_protocol_module(mod: str) -> bool:
    if mod in MAIN_PROTOCOL_MODULES:
        return False
    if mod.startswith("Kafka.Protocol.Generated."):
        return True
    if mod in {
        "Kafka.Protocol.Primitives",
        "Kafka.Protocol.Message",
        "Kafka.Protocol.Wire",
        "Kafka.Protocol.Wire.Codec",
        "Kafka.Protocol.Wire.Primitives",
        "Kafka.Protocol.Wire.SliceVector",
    }:
        return True
    return False


def extract_main_exposed(cabal: str) -> list[str]:
    m = re.search(
        r"^library\n.*?exposed-modules:\n(.*?)\n    build-depends:",
        cabal,
        re.S | re.M,
    )
    if not m:
        raise RuntimeError("could not find main library exposed-modules")
    return [ln.strip() for ln in m.group(1).splitlines() if ln.strip().startswith("Kafka.")]


def replace_main_exposed(cabal: str, modules: list[str]) -> str:
    block = "    exposed-modules:\n" + "".join(f"        {m}\n" for m in modules)
    return re.sub(
        r"^library\n.*?exposed-modules:\n.*?\n    build-depends:",
        lambda m: m.group(0).split("exposed-modules:")[0] + "exposed-modules:\n" + block[20:] + "    build-depends:",
        cabal,
        count=1,
        flags=re.S | re.M,
    )


def add_build_dep(block: str, dep: str) -> str:
    if dep in block:
        return block
    return re.sub(
        r"(    build-depends:\n)",
        rf"\1        {dep},\n",
        block,
        count=1,
    )


def add_wireform_kafka_protocol_dep(cabal: str) -> str:
    dep = "wireform-kafka-protocol         == 0.1.*"
    # main library
    cabal = re.sub(
        r"(^library\n[\s\S]*?    build-depends:\n)",
        lambda m: add_build_dep(m.group(1), dep),
        cabal,
        count=1,
        flags=re.M,
    )
    # stanzas that depend on wireform-kafka
    parts = re.split(r"(?=^(?:test-suite|executable|benchmark|library wireform-kafka) )", cabal, flags=re.M)
    out = [parts[0]]
    for part in parts[1:]:
        if "wireform-kafka-protocol" in part:
            out.append(part)
            continue
        if re.search(r"^\s+wireform-kafka\s*,?\s*$", part, re.M) or re.search(
            r"^\s+wireform-kafka,", part, re.M
        ):
            part = add_build_dep(part, dep)
        out.append(part)
    return "".join(out)


def write_protocol_cabal(modules: list[str]) -> None:
    PROTO_DIR.mkdir(exist_ok=True)
    exposed = "".join(f"        {m}\n" for m in modules)
    content = f"""cabal-version: 3.0
name: wireform-kafka-protocol
version: 0.1.0.0
synopsis: Kafka wire protocol types (codegen message records and wire codec)
description:
    Generated @Kafka.Protocol.Generated.*@ request/response records and the
    @Kafka.Protocol.Wire.*@ codec layer. Depends on the upstream message JSON
    under @wireform-kafka/data/@; regenerate via @scripts/regen-kafka-protocol.sh@.
    Import this package explicitly — @wireform-kafka@ does not re-export it.
license: BSD-3-Clause
license-file: LICENSE
author: Ian Duncan
maintainer: ian@iankduncan.com
copyright: 2026 Ian Duncan
category: Network, Codec
build-type: Simple
tested-with: GHC ==9.6.4 || ==9.8.4

source-repository head
    type: git
    location: https://github.com/iand675/wireform-

common defaults
    ghc-options:
        -Wall
        -Wcompat
        -Widentities
        -Wincomplete-record-updates
        -Wincomplete-uni-patterns
        -Wmissing-export-lists
        -Wpartial-fields
        -Wredundant-constraints
    default-language: GHC2021
    default-extensions:
        OverloadedStrings
        OverloadedRecordDot
        DuplicateRecordFields
        StrictData
        DerivingStrategies
        DeriveAnyClass
        LambdaCase

library
    import: defaults
    hs-source-dirs: ../wireform-kafka/src
    exposed-modules:
{exposed}    build-depends:
        base                      >= 4.16  && < 5,
        bytestring                >= 0.11  && < 0.13,
        containers                >= 0.6   && < 0.9,
        deepseq                   >= 1.4   && < 1.6,
        text                      >= 2.0   && < 2.2,
        uuid                      >= 1.3   && < 1.4,
        vector                    >= 0.13  && < 0.14
"""
    PROTO_CABAL.write_text(content)
    license_src = ROOT / "wireform-kafka" / "LICENSE"
    license_dst = PROTO_DIR / "LICENSE"
    if not license_dst.exists():
        license_dst.write_text(license_src.read_text())


def update_cabal_project() -> None:
    text = CABAL_PROJECT.read_text()
    if "wireform-kafka-protocol/" in text:
        return
    text = text.replace(
        "  wireform-kafka/\n",
        "  wireform-kafka-protocol/\n  wireform-kafka/\n",
    )
    if "package wireform-kafka-protocol" not in text:
        text = text.rstrip() + "\n\npackage wireform-kafka-protocol\n  optimization: 2\n"
    CABAL_PROJECT.write_text(text + "\n")


def main() -> None:
    cabal = KAFKA_CABAL.read_text()
    all_mods = extract_main_exposed(cabal)
    proto_mods = sorted(m for m in all_mods if is_protocol_module(m))
    main_mods = sorted(m for m in all_mods if not is_protocol_module(m))
    write_protocol_cabal(proto_mods)
    cabal = replace_main_exposed(cabal, main_mods)
    cabal = add_wireform_kafka_protocol_dep(cabal)
    # drop stale sublibrary mention in description if present
    cabal = cabal.replace(
        "sublibrary @wireform-kafka-protocol@",
        "package @wireform-kafka-protocol@",
    )
    KAFKA_CABAL.write_text(cabal)
    update_cabal_project()
    print(f"protocol modules: {len(proto_mods)}")
    print(f"main exposed modules: {len(main_mods)}")


if __name__ == "__main__":
    main()
