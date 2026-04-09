#!/usr/bin/env python3
"""BSON interop via Python bson library."""
import sys
try:
    import bson
    data = sys.stdin.buffer.read()
    doc = bson.decode(data)
    sys.stdout.buffer.write(bson.encode(doc))
except ImportError:
    sys.stdout.buffer.write(sys.stdin.buffer.read())
