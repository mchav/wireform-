#!/usr/bin/env python3
"""Insert wireform-kafka-protocol sublibrary into wireform-kafka.cabal."""
from __future__ import annotations

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
CABAL = ROOT / "wireform-kafka" / "wireform-kafka.cabal"
GEN_DIR = ROOT / "wireform-kafka" / "src" / "Kafka" / "Protocol" / "Generated"

PROTOCOL_BASE = [
    "Kafka.Protocol.Primitives",
    "Kafka.Protocol.Message",
    "Kafka.Protocol.Wire",
    "Kafka.Protocol.Wire.Codec",
    "Kafka.Protocol.Wire.Primitives",
    "Kafka.Protocol.Wire.SliceVector",
]


def generated_modules() -> list[str]:
    return sorted(
        f"Kafka.Protocol.Generated.{p.stem}" for p in GEN_DIR.glob("*.hs")
    )


def indent_modules(mods: list[str], indent: str = "        ") -> str:
    return "\n".join(f"{indent}{mod}" for mod in mods)


def main() -> None:
    text = CABAL.read_text()
    if "library wireform-kafka-protocol" in text:
        print("wireform-kafka-protocol already present; run sync-kafka-protocol-cabal-modules.sh")
        return

    text = text.replace(
        "      @kafka-codegen@ executable.\n",
        "      @kafka-codegen@ executable (sublibrary @wireform-kafka-protocol@).\n",
        1,
    )

    generated = generated_modules()
    protocol_modules = PROTOCOL_BASE + generated
    protocol_set = set(protocol_modules)

    lib_match = re.search(
        r"^library\n(.*?)(?=^library wireform-kafka-streams)",
        text,
        re.M | re.S,
    )
    if not lib_match:
        sys.exit("default library stanza not found (expected before wireform-kafka-streams)")

    streams_m = re.search(
        r"^library wireform-kafka-streams",
        text[lib_match.start() :],
        re.M,
    )
    if not streams_m:
        sys.exit("library wireform-kafka-streams not found after default library")

    tail_start = lib_match.start() + streams_m.start()
    lib_body = lib_match.group(1)
    exp_m = re.search(r"    exposed-modules:\n((?:        .+\n)+)", lib_body)
    if not exp_m:
        sys.exit("exposed-modules not found in default library")

    exposed = [ln.strip() for ln in exp_m.group(1).strip().splitlines()]
    main_exposed = [mod for mod in exposed if mod not in protocol_set]

    new_exposed = "    exposed-modules:\n" + indent_modules(main_exposed) + "\n"
    lib_body = lib_body[: exp_m.start()] + new_exposed + lib_body[exp_m.end() :]

    reexport = "    reexported-modules:\n" + indent_modules(protocol_modules) + "\n"
    bd_m = re.search(r"    build-depends:\n", lib_body)
    if not bd_m:
        sys.exit("build-depends not found in default library")
    lib_body = lib_body[: bd_m.start()] + reexport + lib_body[bd_m.start() :]
    lib_body = lib_body.replace(
        "    build-depends:\n        base",
        "    build-depends:\n        wireform-kafka:wireform-kafka-protocol,\n        base",
        1,
    )

    protocol_stanza = (
        "library wireform-kafka-protocol\n"
        "    import: defaults\n"
        "    visibility: public\n"
        "    hs-source-dirs: src\n"
        "    exposed-modules:\n"
        + indent_modules(protocol_modules)
        + "\n"
        "    build-depends:\n"
        "        base                      >= 4.16  && < 5,\n"
        "        bytestring                >= 0.11  && < 0.13,\n"
        "        containers                >= 0.6   && < 0.9,\n"
        "        deepseq                   >= 1.4   && < 1.6,\n"
        "        text                      >= 2.0   && < 2.2,\n"
        "        uuid                      >= 1.3   && < 1.4,\n"
        "        vector                    >= 0.13  && < 0.14\n"
        "\n"
    )

    text = (
        text[: lib_match.start()]
        + protocol_stanza
        + "library\n"
        + lib_body
        + text[tail_start :]
    )
    CABAL.write_text(text)
    print(
        f"Patched {CABAL.name}: {len(protocol_modules)} protocol modules, "
        f"{len(main_exposed)} main exposed"
    )


if __name__ == "__main__":
    main()
