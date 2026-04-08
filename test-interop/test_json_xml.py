#!/usr/bin/env python3
"""XML interop: parse XML with Python's ElementTree, output canonical form."""
import sys, xml.etree.ElementTree as ET
data = sys.stdin.buffer.read()
root = ET.fromstring(data)
ET.indent(root)
sys.stdout.buffer.write(ET.tostring(root, encoding='unicode').encode('utf-8'))
