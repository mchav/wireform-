#!/usr/bin/env python3
"""Avro interop: read wireform-encoded Avro bytes from stdin with schema, decode, print JSON."""
import sys, json, io, avro.io, avro.schema
schema_json = sys.argv[1]
schema = avro.schema.parse(schema_json)
data = sys.stdin.buffer.read()
reader = avro.io.DatumReader(schema)
decoder = avro.io.BinaryDecoder(io.BytesIO(data))
obj = reader.read(decoder)
json.dump(obj, sys.stdout)
