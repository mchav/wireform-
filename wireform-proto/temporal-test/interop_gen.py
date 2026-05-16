#!/usr/bin/env python3
"""Generate and verify test protobuf binary data for Haskell interop.

Uses raw protobuf wire format encoding to avoid version compatibility issues
with the protoc-generated Python code.
"""
import struct
import sys
import os

def encode_varint(value):
    result = bytearray()
    while value > 0x7f:
        result.append((value & 0x7f) | 0x80)
        value >>= 7
    result.append(value & 0x7f)
    return bytes(result)

def encode_tag(field_number, wire_type):
    return encode_varint((field_number << 3) | wire_type)

def encode_len_delimited(field_number, data):
    tag = encode_tag(field_number, 2)
    return tag + encode_varint(len(data)) + data

def encode_varint_field(field_number, value):
    return encode_tag(field_number, 0) + encode_varint(value)

def generate_test_data():
    # DataBlob: encoding_type=1 (PROTO3), data=b"hello from python"
    blob = encode_varint_field(1, 1) + encode_len_delimited(2, b"hello from python")
    with open('/tmp/interop_datablob.bin', 'wb') as f:
        f.write(blob)
    print(f"Python: wrote DataBlob ({len(blob)} bytes)")

    # WorkflowExecution: workflow_id="wf-python-test-123", run_id="run-python-abc-456"
    wf = encode_len_delimited(1, b"wf-python-test-123") + encode_len_delimited(2, b"run-python-abc-456")
    with open('/tmp/interop_wfexec.bin', 'wb') as f:
        f.write(wf)
    print(f"Python: wrote WorkflowExecution ({len(wf)} bytes)")

def decode_varint(data, offset):
    result = 0
    shift = 0
    while True:
        byte = data[offset]
        offset += 1
        result |= (byte & 0x7f) << shift
        if (byte & 0x80) == 0:
            break
        shift += 7
    return result, offset

def decode_field(data, offset):
    tag, offset = decode_varint(data, offset)
    field_number = tag >> 3
    wire_type = tag & 0x7
    if wire_type == 0:
        value, offset = decode_varint(data, offset)
        return field_number, value, offset
    elif wire_type == 2:
        length, offset = decode_varint(data, offset)
        value = data[offset:offset+length]
        return field_number, value, offset + length
    else:
        raise ValueError(f"Unsupported wire type {wire_type}")

def verify_haskell_data():
    ok = True

    if os.path.exists('/tmp/interop_hs_datablob.bin'):
        with open('/tmp/interop_hs_datablob.bin', 'rb') as f:
            data = f.read()
        offset = 0
        fields = {}
        while offset < len(data):
            fn, val, offset = decode_field(data, offset)
            fields[fn] = val

        # encoding_type = field 1 (varint), should be 2 (ENCODING_TYPE_JSON)
        assert fields.get(1) == 2, f"Expected encoding_type=2, got {fields.get(1)}"
        # data = field 2 (bytes)
        assert fields.get(2) == b"hello from haskell", f"Expected data='hello from haskell', got {fields.get(2)}"
        print("Python: verified Haskell DataBlob OK")
    else:
        print("Python: no Haskell DataBlob found")
        ok = False

    if os.path.exists('/tmp/interop_hs_wfexec.bin'):
        with open('/tmp/interop_hs_wfexec.bin', 'rb') as f:
            data = f.read()
        offset = 0
        fields = {}
        while offset < len(data):
            fn, val, offset = decode_field(data, offset)
            fields[fn] = val

        assert fields.get(1) == b"hs-workflow-999", f"Expected workflow_id='hs-workflow-999', got {fields.get(1)}"
        assert fields.get(2) == b"hs-run-888", f"Expected run_id='hs-run-888', got {fields.get(2)}"
        print("Python: verified Haskell WorkflowExecution OK")
    else:
        print("Python: no Haskell WorkflowExecution found")
        ok = False

    return ok

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'verify':
        if verify_haskell_data():
            print("\nPython verification: ALL PASSED")
        else:
            print("\nPython verification: FAILED")
            sys.exit(1)
    else:
        generate_test_data()
