#!/usr/bin/env python3
"""Length-prefixed stdin/stdout driver for the wireform-fory
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

import dataclasses
import numpy as np
import pyfory

fory = pyfory.Fory(xlang=True, ref=False)
fory_ref = pyfory.Fory(xlang=True, ref=True)


# ---------------------------------------------------------------------------
# Registered structs
# ---------------------------------------------------------------------------

@dataclasses.dataclass
class Person:
    name: str
    age: int


@dataclasses.dataclass
class Point:
    x: int
    y: int


fory.register(Person, typename="example.Person")
fory.register(Point,  typename="geom.Point")


_NDARRAY_DTYPES = {
    "int8": np.int8, "int16": np.int16, "int32": np.int32, "int64": np.int64,
    "uint8": np.uint8, "uint16": np.uint16, "uint32": np.uint32, "uint64": np.uint64,
    "float32": np.float32, "float64": np.float64,
    "bool": np.bool_,
}


def to_json_value(v: Any) -> Any:
    if isinstance(v, Person):
        return {"__struct__": "example.Person",
                "fields": {"name": v.name, "age": v.age}}
    if isinstance(v, Point):
        return {"__struct__": "geom.Point",
                "fields": {"x": v.x, "y": v.y}}
    if isinstance(v, bool):
        return v
    if isinstance(v, np.ndarray):
        return {"__ndarray__": {
            "dtype": v.dtype.name,
            "values": [to_json_value(x.item()) for x in v],
        }}
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
        if set(j.keys()) == {"__ndarray__"}:
            spec = j["__ndarray__"]
            dtype = _NDARRAY_DTYPES[spec["dtype"]]
            values = [from_json_value(x) for x in spec["values"]]
            return np.array(values, dtype=dtype)
        if set(j.keys()) >= {"__struct__"}:
            tn = j["__struct__"]
            fields = j["fields"]
            if tn == "example.Person":
                return Person(name=fields["name"], age=fields["age"])
            if tn == "geom.Point":
                return Point(x=fields["x"], y=fields["y"])
            raise ValueError(f"unknown struct typename {tn}")
        return {k: from_json_value(v) for k, v in j.items()}
    if isinstance(j, list):
        return [from_json_value(x) for x in j]
    raise ValueError(f"unsupported json value {j!r}")


def from_json_value_shared(j: Any, shared_pool: dict) -> Any:
    """Like from_json_value but additionally honours
    {"__shared__": <id>, "value": <inner>} wrappers, which build
    a single Python object whose references are reused across the
    JSON tree. This lets the test driver produce inputs where
    pyfory's reference tracking actually kicks in."""
    if isinstance(j, dict) and set(j.keys()) >= {"__shared__"}:
        sid = j["__shared__"]
        if sid in shared_pool:
            return shared_pool[sid]
        inner = from_json_value_shared(j["value"], shared_pool)
        shared_pool[sid] = inner
        return inner
    if isinstance(j, list):
        return [from_json_value_shared(x, shared_pool) for x in j]
    if isinstance(j, dict):
        if set(j.keys()) == {"__bytes__"}:
            return base64.b64decode(j["__bytes__"].encode("ascii"))
        if set(j.keys()) == {"__float__"}:
            return float(j["__float__"])
        if set(j.keys()) == {"__ndarray__"}:
            spec = j["__ndarray__"]
            dtype = _NDARRAY_DTYPES[spec["dtype"]]
            values = [from_json_value_shared(x, shared_pool) for x in spec["values"]]
            return np.array(values, dtype=dtype)
        return {k: from_json_value_shared(v, shared_pool) for k, v in j.items()}
    return from_json_value(j)


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
            elif mode == "R":
                # Mode 'R': ref-tracking decode (Haskell -> Python).
                obj = fory_ref.deserialize(payload)
                jv = to_json_value(obj)
                write_response("K", json.dumps(jv).encode("utf-8"))
            elif mode == "S":
                # Mode 'S': ref-tracking encode (Python -> Haskell).
                # The JSON description supports a special wrapper
                # {"__shared__": <inner>} which decodes to a Python
                # object that's referenced multiple times in the
                # outer container, so pyfory's ref tracking actually
                # kicks in when serializing.
                jv = json.loads(payload.decode("utf-8"))
                obj = from_json_value_shared(jv, {})
                bs = fory_ref.serialize(obj)
                write_response("K", bs)
            else:
                write_response("E", f"unknown mode {mode}".encode("utf-8"))
        except Exception:
            write_response("E", traceback.format_exc().encode("utf-8"))


if __name__ == "__main__":
    try:
        main()
    except SystemExit:
        raise
    except BaseException:
        sys.stderr.write(
            "[driver.py top-level exception]\n" + traceback.format_exc()
        )
        sys.stderr.flush()
        sys.exit(2)
