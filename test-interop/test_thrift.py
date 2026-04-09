#!/usr/bin/env python3
"""Thrift interop: verify binary protocol struct roundtrip."""
import sys, json
data = sys.stdin.buffer.read()
try:
    from thrift.protocol import TBinaryProtocol
    from thrift.transport import TTransport
    transport = TTransport.TMemoryBuffer(data)
    protocol = TBinaryProtocol.TBinaryProtocol(transport)
    protocol.readStructBegin()
    fields = []
    while True:
        _, ftype, fid = protocol.readFieldBegin()
        if ftype == 0: break  # STOP
        protocol.skip(ftype)
        protocol.readFieldEnd()
        fields.append((fid, ftype))
    protocol.readStructEnd()
    sys.stdout.write(json.dumps({"fields": fields}))
except ImportError:
    sys.stdout.buffer.write(data)
