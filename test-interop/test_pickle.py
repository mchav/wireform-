#!/usr/bin/env python3
"""Pickle interop: decode wireform pickle, re-encode."""
import sys, pickle
data = sys.stdin.buffer.read()
obj = pickle.loads(data)
sys.stdout.buffer.write(pickle.dumps(obj, protocol=2))
