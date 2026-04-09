#!/usr/bin/env python3
"""CBOR interop: read wireform-encoded bytes from stdin, decode, re-encode, write to stdout."""
import sys, cbor2
data = sys.stdin.buffer.read()
obj = cbor2.loads(data)
sys.stdout.buffer.write(cbor2.dumps(obj))
