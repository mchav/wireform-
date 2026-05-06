#!/usr/bin/env python3
"""Companion micro-benchmark for wireform-fory's encoder /
decoder, run against the same payload set the Haskell criterion
benchmark uses (bench/Bench.hs).

Usage:
    python3 bench.py             # runs 12 encode + 12 decode benches
    python3 bench.py --json out.json   # also dumps machine-readable results

Each benchmark times N iterations of encode (or decode) of a
fixed payload and reports ns/op (median over a small sample of
inner-loop runs).
"""
from __future__ import annotations

import argparse
import dataclasses
import json
import statistics
import sys
import time
from typing import Any, Callable

import numpy as np
import pyfory


# ---------------------------------------------------------------------------
# Registered structs
# ---------------------------------------------------------------------------

@dataclasses.dataclass
class Person:
    name: str
    age: int


fory = pyfory.Fory(xlang=True, ref=False)
fory.register(Person, typename="example.Person")


# ---------------------------------------------------------------------------
# Payload set (must match bench/Bench.hs)
# ---------------------------------------------------------------------------

p_int               = 1234567890
p_float             = 3.141592653589793
p_small_str         = "a" * 12
p_long_str          = "a" * 1024
p_bytes_1k          = b"\x42" * 1024
p_list_of_int       = [i for i in range(100)]
p_list_of_string    = ["x" * 8 for _ in range(100)]
p_map_str_int       = {f"k{i}": i for i in range(50)}
p_int32_array_1k    = np.array([i for i in range(1024)], dtype=np.int32)
p_float64_array_1k  = np.array([i * 0.5 for i in range(1024)], dtype=np.float64)
p_person            = Person("alice", 30)
p_list_of_person    = [Person(f"user{i}", i) for i in range(100)]


PAYLOADS: list[tuple[str, Any]] = [
    ("int",               p_int),
    ("float",             p_float),
    ("small string",      p_small_str),
    ("long string 1k",    p_long_str),
    ("bytes 1k",          p_bytes_1k),
    ("list-of-int 100",   p_list_of_int),
    ("list-of-string 100", p_list_of_string),
    ("map str/int 50",    p_map_str_int),
    ("int32-array 1k",    p_int32_array_1k),
    ("float64-array 1k",  p_float64_array_1k),
    ("struct Person",     p_person),
    ("list-of-struct 100", p_list_of_person),
]


# ---------------------------------------------------------------------------
# Timer
# ---------------------------------------------------------------------------

def time_call(action: Callable[[], Any], inner: int) -> float:
    """Return the elapsed seconds for `inner` calls of `action`."""
    t0 = time.perf_counter()
    for _ in range(inner):
        action()
    t1 = time.perf_counter()
    return t1 - t0


def auto_inner(action: Callable[[], Any]) -> int:
    """Pick an inner-loop count so a single sample takes >= 50 ms."""
    inner = 16
    while True:
        elapsed = time_call(action, inner)
        if elapsed >= 0.05 or inner >= 1_000_000:
            return inner
        inner *= 2


def bench(name: str, action: Callable[[], Any]) -> dict:
    inner = auto_inner(action)
    samples = [time_call(action, inner) / inner for _ in range(7)]
    samples.sort()
    median = samples[len(samples) // 2]
    return {
        "name":   name,
        "inner":  inner,
        "ns/op":  median * 1e9,
        "stddev": statistics.pstdev(samples) * 1e9,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--json", help="dump JSON results to this path")
    args = ap.parse_args()

    encode_results = []
    print("=== encode ===")
    for name, value in PAYLOADS:
        # Warm up: serialize once so the first call's compilation
        # cost doesn't pollute the timed loop.
        _ = fory.serialize(value)
        r = bench(name, lambda v=value: fory.serialize(v))
        bytes_len = len(fory.serialize(value))
        r["bytes"] = bytes_len
        encode_results.append(r)
        print(f"  {name:<24} {r['ns/op']:>12.1f} ns/op   "
              f"({bytes_len} bytes, inner={r['inner']})")

    print("\n=== decode ===")
    decode_results = []
    for name, value in PAYLOADS:
        bs = fory.serialize(value)
        _ = fory.deserialize(bs)
        r = bench(name, lambda b=bs: fory.deserialize(b))
        r["bytes"] = len(bs)
        decode_results.append(r)
        print(f"  {name:<24} {r['ns/op']:>12.1f} ns/op   "
              f"({len(bs)} bytes, inner={r['inner']})")

    if args.json:
        with open(args.json, "w") as fp:
            json.dump(
                {"encode": encode_results, "decode": decode_results},
                fp, indent=2,
            )
        print(f"\nWrote {args.json}")


if __name__ == "__main__":
    main()
