#!/usr/bin/env python3
"""Parse Autobahn|Testsuite fuzzingclient index.json and print a summary.

Exits non-zero if any case failed (status != OK and != NON-STRICT,
which Autobahn classifies as "informational").  Case behaviour vocab:

    OK            -- passed strictly
    NON-STRICT    -- passed under the looser interpretation
    INFORMATIONAL -- recorded for review only
    UNIMPLEMENTED -- case was not run (excluded in spec)
    WRONG CODE    -- close code mismatch
    FAILED        -- handshake-level or framing-level miss

The first two are treated as pass; everything else as fail.
"""
import json
import os
import sys
from collections import Counter

PASS = {"OK", "INFORMATIONAL", "NON-STRICT"}

def main(argv):
    if len(argv) != 2:
        print("usage: autobahn-summary.py REPORTS/servers/index.json",
              file=sys.stderr)
        return 2
    path = argv[1]
    if not os.path.exists(path):
        print(f"report not found: {path}", file=sys.stderr)
        return 2
    with open(path) as fh:
        data = json.load(fh)

    overall = Counter()
    section_totals = {}
    failures = []
    for agent, cases in data.items():
        for case_id, info in cases.items():
            behavior = info.get("behavior", "?")
            overall[behavior] += 1
            section = case_id.split(".", 1)[0]
            sec_counter = section_totals.setdefault(section, Counter())
            sec_counter[behavior] += 1
            if behavior not in PASS:
                failures.append((case_id, behavior,
                                 info.get("reportfile", "")))

    total = sum(overall.values())
    passing = sum(overall[k] for k in PASS if k in overall)
    fail_count = total - passing

    print("=" * 60)
    print(f"Autobahn|Testsuite fuzzingclient report ({path})")
    print("=" * 60)
    for section in sorted(section_totals, key=lambda s: int(s)
                          if s.isdigit() else 99):
        counts = section_totals[section]
        ssum = sum(counts.values())
        spass = sum(counts[k] for k in PASS if k in counts)
        print(f"  Section {section:>2}: {spass:>3} / {ssum:<3} passed"
              + (f"   ({dict(counts)})" if spass != ssum else ""))
    print("-" * 60)
    print(f"  TOTAL:     {passing:>3} / {total:<3} passed")
    print(f"  Breakdown: {dict(overall)}")
    print("=" * 60)
    if failures:
        print(f"\n{len(failures)} failing case(s):")
        for case_id, behavior, report in failures:
            print(f"  {case_id:<10} {behavior}  ({report})")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
