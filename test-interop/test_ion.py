#!/usr/bin/env python3
"""Ion interop via amazon.ion."""
import sys
try:
    import amazon.ion.simpleion as ion
    data = sys.stdin.buffer.read()
    obj = ion.loads(data, single_value=True)
    sys.stdout.buffer.write(ion.dumps(obj, binary=True))
except ImportError:
    sys.stdout.buffer.write(sys.stdin.buffer.read())
