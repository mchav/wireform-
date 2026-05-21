#!/usr/bin/env bash
# Sync wireform-kafka-protocol exposed-modules in wireform-kafka.cabal
# from the generated Haskell modules on disk.
#
# Run after scripts/regen-kafka-protocol.sh adds or removes
# Kafka.Protocol.Generated.* modules.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CABAL="$PROJECT_ROOT/wireform-kafka/wireform-kafka.cabal"
GEN_DIR="$PROJECT_ROOT/wireform-kafka/src/Kafka/Protocol/Generated"

if [[ ! -f "$CABAL" ]]; then
  echo "error: $CABAL not found" >&2
  exit 1
fi

python3 - "$CABAL" "$GEN_DIR" << 'PY'
import pathlib
import re
import sys

cabal_path = pathlib.Path(sys.argv[1])
gen_dir = pathlib.Path(sys.argv[2])

generated = sorted(
    f"Kafka.Protocol.Generated.{p.stem}"
    for p in gen_dir.glob("*.hs")
)

protocol_base = [
    "Kafka.Protocol.Primitives",
    "Kafka.Protocol.Message",
    "Kafka.Protocol.Wire",
    "Kafka.Protocol.Wire.Codec",
    "Kafka.Protocol.Wire.Primitives",
    "Kafka.Protocol.Wire.SliceVector",
]
protocol_modules = protocol_base + generated

def indent_modules(mods, indent="        "):
    return "\n".join(f"{indent}{mod}" for mod in mods)

text = cabal_path.read_text()

proto_match = re.search(
    r"^library wireform-kafka-protocol\n(.*?)(?=^library |\Z)",
    text,
    re.M | re.S,
)
if not proto_match:
    raise SystemExit("library wireform-kafka-protocol stanza not found")

proto_body = proto_match.group(1)
exp_m = re.search(r"    exposed-modules:\n(?:        .+\n)+", proto_body)
if not exp_m:
    raise SystemExit("wireform-kafka-protocol exposed-modules not found")

new_exp = "    exposed-modules:\n" + indent_modules(protocol_modules) + "\n"
proto_body_new = proto_body[:exp_m.start()] + new_exp + proto_body[exp_m.end():]
text = text[:proto_match.start(1)] + proto_body_new + text[proto_match.end(1):]

default_m = re.search(
    r"^library\n    import: defaults\n    hs-source-dirs: src\n    include-dirs:",
    text,
    re.M,
)
if not default_m:
    raise SystemExit("default wireform-kafka library stanza not found")

streams_m = re.search(r"^library wireform-kafka-streams", text[default_m.start():], re.M)
if not streams_m:
    raise SystemExit("library wireform-kafka-streams not found")

lib_start = default_m.start()
lib_body_start = default_m.start() + len("library\n")
lib_body_end = default_m.start() + streams_m.start()
lib_body = text[lib_body_start:lib_body_end]

rex_m = re.search(r"    reexported-modules:\n(?:        .+\n)+", lib_body)
if not rex_m:
    raise SystemExit("default library reexported-modules not found")

new_rex = "    reexported-modules:\n" + indent_modules(protocol_modules) + "\n"
lib_body_new = lib_body[:rex_m.start()] + new_rex + lib_body[rex_m.end():]
text = text[:lib_body_start] + lib_body_new + text[lib_body_end:]

cabal_path.write_text(text)
print(f"Synced {len(protocol_modules)} modules ({len(generated)} generated) in {cabal_path}")
PY
