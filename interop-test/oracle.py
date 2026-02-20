#!/usr/bin/env python3
"""
Protobuf interop oracle: the reference implementation for conformance testing.

Modes (passed as first argument):

  roundtrip <MessageType>
    Read binary protobuf from stdin, decode using Python's protobuf library,
    re-encode deterministically, write binary to stdout.
    Exit code 0 = success, 1 = decode error.

  encode <MessageType> <JSON>
    Parse the JSON into the given message type, encode to binary,
    write to stdout.

  decode <MessageType>
    Read binary protobuf from stdin, decode, print field values as
    newline-separated key=value pairs to stdout (for Haskell to verify).

  fields <MessageType>
    Read binary from stdin, decode, print each field as "name=value" lines.
"""

import sys
import json
import struct

# Add the interop-test directory to the path so we can import the generated module.
sys.path.insert(0, sys.path[0] if sys.path[0] else '.')
import interop_pb2

MESSAGE_TYPES = {
    'Scalars': interop_pb2.Scalars,
    'Nested': interop_pb2.Nested,
    'Repeated': interop_pb2.Repeated,
    'MapFields': interop_pb2.MapFields,
    'OneofMsg': interop_pb2.OneofMsg,
    'TestCase': interop_pb2.TestCase,
}


def roundtrip(msg_type_name):
    """Read binary, decode, re-encode, write binary."""
    cls = MESSAGE_TYPES[msg_type_name]
    data = sys.stdin.buffer.read()
    msg = cls()
    msg.ParseFromString(data)
    out = msg.SerializeToString()
    sys.stdout.buffer.write(out)


def decode_fields(msg_type_name):
    """Read binary, decode, print field values."""
    cls = MESSAGE_TYPES[msg_type_name]
    data = sys.stdin.buffer.read()
    msg = cls()
    msg.ParseFromString(data)
    print_fields(msg, '')


def print_fields(msg, prefix):
    """Recursively print field values."""
    descriptor = msg.DESCRIPTOR
    for field in descriptor.fields:
        value = getattr(msg, field.name)
        full_name = f"{prefix}{field.name}" if prefix == '' else f"{prefix}.{field.name}"

        if field.message_type and field.label == field.LABEL_REPEATED:
            if field.message_type.GetOptions().map_entry:
                # Map field
                for k, v in sorted(value.items()):
                    if hasattr(v, 'DESCRIPTOR'):
                        print(f"{full_name}[{k}]=<msg>")
                    else:
                        print(f"{full_name}[{k}]={v}")
            else:
                # Repeated message
                for i, item in enumerate(value):
                    if hasattr(item, 'DESCRIPTOR'):
                        print_fields(item, f"{full_name}[{i}]")
                    else:
                        print(f"{full_name}[{i}]={item}")
        elif field.label == field.LABEL_REPEATED:
            # Repeated scalar
            for i, item in enumerate(value):
                print(f"{full_name}[{i}]={item}")
        elif field.message_type:
            if msg.HasField(field.name):
                print_fields(value, full_name)
        elif field.type == field.TYPE_BYTES:
            print(f"{full_name}={value.hex()}")
        elif field.type == field.TYPE_BOOL:
            print(f"{full_name}={'true' if value else 'false'}")
        elif field.type == field.TYPE_FLOAT or field.type == field.TYPE_DOUBLE:
            print(f"{full_name}={repr(value)}")
        elif field.type == field.TYPE_STRING:
            print(f"{full_name}={value}")
        else:
            print(f"{full_name}={value}")


def encode_from_json(msg_type_name, json_str):
    """Parse JSON into a message and encode to binary."""
    from google.protobuf.json_format import Parse
    cls = MESSAGE_TYPES[msg_type_name]
    msg = cls()
    Parse(json_str, msg)
    sys.stdout.buffer.write(msg.SerializeToString())


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: oracle.py <mode> <MessageType> [json]", file=sys.stderr)
        sys.exit(2)

    mode = sys.argv[1]
    msg_type = sys.argv[2]

    if msg_type not in MESSAGE_TYPES:
        print(f"Unknown message type: {msg_type}", file=sys.stderr)
        sys.exit(2)

    if mode == 'roundtrip':
        roundtrip(msg_type)
    elif mode == 'decode' or mode == 'fields':
        decode_fields(msg_type)
    elif mode == 'encode':
        if len(sys.argv) < 4:
            print("encode mode requires JSON argument", file=sys.stderr)
            sys.exit(2)
        encode_from_json(msg_type, sys.argv[3])
    else:
        print(f"Unknown mode: {mode}", file=sys.stderr)
        sys.exit(2)
