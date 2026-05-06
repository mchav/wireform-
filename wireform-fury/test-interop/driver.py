#!/usr/bin/env python3
"""Length-prefixed stdin/stdout driver for the wireform-fury
interop test suite. Reads (mode, len, payload) frames, replies
with (status, len, payload) frames.

mode 'D': payload is fory-encoded bytes; we decode via pyfory and
          return a JSON description of the decoded value.
mode 'E': payload is a JSON description; we materialise it into
          Python objects and re-encode via pyfory, returning the
          encoded bytes.

JSON description conventions:
- numbers, strings, bools, null, lists, dicts pass through.
- bytes are encoded as {"__bytes__": "<base64>"}.

Status:
'K' = ok, payload is the response.
'E' = error, payload is a UTF-8 error message.
"""
from __future__ import annotations

import base64
import json
import struct
import sys
import traceback
from typing import Any

import pyfory

fory = pyfory.Fory(xlang=True, ref=False)


def to_json_value(v: Any) -> Any:
    if isinstance(v, bool):
        return v
    if isinstance(v, float):
        return {"__float__": v}
    if isinstance(v, bytes):
        return {"__bytes__": base64.b64encode(v).decode("ascii")}
    if isinstance(v, dict):
        # JSON object keys must be strings; pyfory may produce
        # non-string keys (ints etc). Skip those for now and just
        # preserve string-keyed maps for the interop subset.
        out = {}
        for k, vv in v.items():
            if isinstance(k, str):
                out[k] = to_json_value(vv)
            else:
                out[json.dumps(to_json_value(k))] = to_json_value(vv)
        return out
    if isinstance(v, list) or isinstance(v, tuple):
        return [to_json_value(x) for x in v]
    if isinstance(v, set) or isinstance(v, frozenset):
        return [to_json_value(x) for x in v]
    return v


def from_json_value(j: Any) -> Any:
    if j is None:
        return None
    if isinstance(j, bool):
        return j
    if isinstance(j, int):
        return j
    if isinstance(j, float):
        return j
    if isinstance(j, str):
        return j
    if isinstance(j, dict):
        if set(j.keys()) == {"__bytes__"}:
            return base64.b64decode(j["__bytes__"].encode("ascii"))
        if set(j.keys()) == {"__float__"}:
            return float(j["__float__"])
        return {k: from_json_value(v) for k, v in j.items()}
    if isinstance(j, list):
        return [from_json_value(x) for x in j]
    raise ValueError(f"unsupported json value {j!r}")


def read_exact(n: int) -> bytes:
    out = b""
    while len(out) < n:
        chunk = sys.stdin.buffer.read(n - len(out))
        if not chunk:
            return out
        out += chunk
    return out


def write_response(status: str, payload: bytes) -> None:
    sys.stdout.buffer.write(status.encode("ascii"))
    sys.stdout.buffer.write(struct.pack("<I", len(payload)))
    sys.stdout.buffer.write(payload)
    sys.stdout.buffer.flush()


def main() -> None:
    while True:
        mode_b = read_exact(1)
        if not mode_b:
            return
        mode = mode_b.decode("ascii")
        len_b = read_exact(4)
        if len(len_b) < 4:
            return
        n = struct.unpack("<I", len_b)[0]
        payload = read_exact(n)
        try:
            if mode == "D":
                obj = fory.deserialize(payload)
                jv = to_json_value(obj)
                write_response("K", json.dumps(jv).encode("utf-8"))
            elif mode == "E":
                jv = json.loads(payload.decode("utf-8"))
                obj = from_json_value(jv)
                bs = fory.serialize(obj)
                write_response("K", bs)
            else:
                write_response("E", f"unknown mode {mode}".encode("utf-8"))
        except Exception:
            write_response("E", traceback.format_exc().encode("utf-8"))


if __name__ == "__main__":
    main()
