#!/usr/bin/env python3
"""MsgPack interop: read wireform-encoded bytes from stdin, decode, re-encode, write to stdout."""
import sys, msgpack
data = sys.stdin.buffer.read()
obj = msgpack.unpackb(data, raw=False)
sys.stdout.buffer.write(msgpack.packb(obj, use_bin_type=True))
