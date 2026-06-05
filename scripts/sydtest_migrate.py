#!/usr/bin/env python3
"""Mechanical tasty -> sydtest migration helper.

Strategy: preserve the existing list/layout structure by turning
  testGroup "X" [ a, b ]  ->  describe "X" $ sequence_ [ a, b ]
and swapping the leaf combinators/operators. This avoids the risky
list-to-do-block reflow. Compilation is the source of truth; anything
this script can't safely handle is reported so it can be done by hand.
"""
import re
import sys

# Constructs we don't auto-handle; flag the file for manual review.
SUSPECT = [
    "withResource",
    "localOption",
    "goldenVsString",
    "goldenVsFile",
    "askOption",
    "adjustOption",
    "testGroupWith",
    "after ",
    "expectFail",
    "ingredients",
]


def migrate(src: str) -> tuple[str, list[str]]:
    notes: list[str] = []
    lines = src.splitlines(keepends=True)
    out = []
    saw_hedgehog = "Test.Tasty.Hedgehog" in src or "testProperty" in src
    for line in lines:
        stripped = line.strip()
        # --- import rewrites ---
        if re.match(r"\s*import\s+Test\.Tasty\b", line):
            if "Test.Tasty.HUnit" in line:
                continue  # covered by Test.Syd
            if "Test.Tasty.Hedgehog" in line:
                continue  # replaced by Test.Syd.Hedgehog orphan instances
            if "Test.Tasty.QuickCheck" in line:
                continue  # sydtest has first-class QuickCheck
            # plain Test.Tasty import (testGroup/defaultMain/TestTree/...)
            out.append("import Test.Syd\n")
            continue
        out.append(line)
    src = "".join(out)

    if saw_hedgehog and "import Test.Syd.Hedgehog" not in src:
        # add the orphan-instance import right after the Test.Syd import
        src = re.sub(
            r"(import Test\.Syd\n)",
            r"\1import Test.Syd.Hedgehog ()\n",
            src,
            count=1,
        )

    # type signatures
    src = src.replace(":: TestTree", ":: Spec")
    src = re.sub(r"\bTestTree\b", "Spec", src)

    # combinators
    src = re.sub(r"\btestGroup\b(\s+)", r"describe\1", src)
    # after the describe name, a list follows -> need `$ sequence_`
    # handled below by inserting sequence_ before the bracket.

    src = re.sub(r"\btestCase\b", "it", src)
    src = re.sub(r"\btestProperty\b", "it", src)

    # operators / assertions. These tokens only ever appear as the
    # HUnit assertion operators, so a bare token replace is safe and
    # also catches the multi-line `expr @?=\n  expr` form.
    if "@=?" in src:
        notes.append("had @=? (expected/actual order flipped vs @?=)")
    src = src.replace("@?=", "`shouldBe`")
    src = src.replace("@=?", "`shouldBe`")

    src = _replace_ident(src, "assertFailure", "expectationFailure")
    # tasty's testCase forced the body to IO (); sydtest's `it` accepts
    # many IsTest instances, so a polymorphic `fail`/`pure ()` body is
    # ambiguous. expectationFailure :: String -> IO a pins it to IO.
    src = _replace_ident(src, "fail", "expectationFailure")
    src, n3 = _convert_assert_bool(src)
    notes += n3
    if "assertEqual" in src:
        notes.append("contains assertEqual (needs manual conversion)")

    out, notes2 = _wrap_describe_lists(src)
    notes += notes2

    for s in SUSPECT:
        if s in out:
            notes.append(f"contains {s!r}")
    return out, notes


def _replace_ident(src: str, ident: str, repl: str) -> str:
    """Replace whole-word `ident` with `repl`, skipping string/char
    literals and -- / {- -} comments."""
    out = []
    i = 0
    n = len(src)
    while i < n:
        c = src[i]
        # line comment
        if c == "-" and i + 1 < n and src[i + 1] == "-":
            j = src.find("\n", i)
            if j == -1:
                out.append(src[i:])
                break
            out.append(src[i : j + 1])
            i = j + 1
            continue
        # block comment (no nesting needed for our inputs, but handle depth)
        if c == "{" and i + 1 < n and src[i + 1] == "-":
            depth = 1
            j = i + 2
            while j < n and depth > 0:
                if src[j] == "{" and j + 1 < n and src[j + 1] == "-":
                    depth += 1
                    j += 2
                elif src[j] == "-" and j + 1 < n and src[j + 1] == "}":
                    depth -= 1
                    j += 2
                else:
                    j += 1
            out.append(src[i:j])
            i = j
            continue
        # string literal
        if c == '"':
            j = _read_string_literal(src, i)
            if j == -1:
                out.append(src[i:])
                break
            out.append(src[i:j])
            i = j
            continue
        # char literal: '\n' '"' 'a' etc. Be conservative: only treat as
        # char lit when it looks like one to avoid eating ' in names.
        if c == "'" and i + 2 < n:
            if src[i + 1] == "\\":
                k = src.find("'", i + 2)
                if k != -1 and k - i <= 5:
                    out.append(src[i : k + 1])
                    i = k + 1
                    continue
            elif i + 2 < n and src[i + 2] == "'":
                out.append(src[i : i + 3])
                i = i + 3
                continue
        # identifier match
        if (c.isalpha() or c == "_") and src.startswith(ident, i):
            before_ok = (i == 0) or not (src[i - 1].isalnum() or src[i - 1] in "_'")
            end = i + len(ident)
            after_ok = end >= n or not (src[end].isalnum() or src[end] in "_'")
            if before_ok and after_ok:
                out.append(repl)
                i = end
                continue
        out.append(c)
        i += 1
    return "".join(out)


def _match_balanced(src: str, i: int) -> int:
    """Given src[i] == '(', return index just past the matching ')'."""
    depth = 0
    while i < len(src):
        c = src[i]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return i + 1
        i += 1
    return -1


def _read_string_literal(src: str, i: int) -> int:
    """Given src[i] == '\"', return index just past the closing quote."""
    i += 1
    while i < len(src):
        if src[i] == "\\":
            i += 2
            continue
        if src[i] == '"':
            return i + 1
        i += 1
    return -1


def _convert_assert_bool(src: str) -> tuple[str, list[str]]:
    """assertBool MSG (EXPR)  ->  (EXPR) `shouldBe` True.

    Drops the bespoke message (the enclosing `it` label carries intent).
    Requires the argument after the message to be a parenthesised expr.
    """
    notes: list[str] = []
    out = []
    i = 0
    needle = "assertBool"
    while True:
        j = src.find(needle, i)
        if j == -1:
            out.append(src[i:])
            break
        # ensure it's a standalone token
        prev = src[j - 1] if j > 0 else " "
        if prev.isalnum() or prev == "_" or prev == "'":
            out.append(src[i : j + len(needle)])
            i = j + len(needle)
            continue
        out.append(src[i:j])
        k = j + len(needle)
        # skip ws
        while k < len(src) and src[k] in " \t\n":
            k += 1
        if k >= len(src) or src[k] != '"':
            notes.append("assertBool without string-literal message; skipped")
            out.append(src[j:k])
            i = k
            continue
        kend = _read_string_literal(src, k)
        if kend == -1:
            out.append(src[j:k])
            i = k
            continue
        # skip ws to expr
        e = kend
        while e < len(src) and src[e] in " \t\n":
            e += 1
        if e >= len(src) or src[e] != "(":
            notes.append("assertBool arg not parenthesised; skipped")
            out.append(src[j:e])
            i = e
            continue
        eend = _match_balanced(src, e)
        if eend == -1:
            out.append(src[j:e])
            i = e
            continue
        expr = src[e:eend]
        out.append(f"{expr} `shouldBe` True")
        i = eend
    return "".join(out), notes


def _wrap_describe_lists(src: str) -> tuple[str, list[str]]:
    """Insert `$ sequence_` between a `describe "..."` and a following `[`.

    Handles both `describe "x"\n  [ ... ]` and `describe "x" [ ... ]`.
    Leaves `describe "x" $ do` untouched.
    """
    notes: list[str] = []
    # describe "..." possibly with trailing spaces/newline then a '['
    pattern = re.compile(
        r'(describe\s+("(?:[^"\\]|\\.)*"|\S+))(\s*)(\n?\s*)\[',
    )

    def repl(m: re.Match) -> str:
        head, _name, sp, nl = m.group(1), m.group(2), m.group(3), m.group(4)
        return f"{head} $ sequence_{sp}{nl}["

    return pattern.sub(repl, src), notes


def migrate_main(src: str) -> str:
    """Migrate a Main.hs entry point."""
    s = migrate(src)[0]
    # defaultMain X  ->  sydTest X    (sydTest takes a Spec)
    s = re.sub(r"\bdefaultMain\b", "sydTest", s)
    return s


if __name__ == "__main__":
    mode = sys.argv[1]
    path = sys.argv[2]
    with open(path) as f:
        src = f.read()
    if mode == "main":
        new = migrate_main(src)
        notes = migrate(src)[1]
    else:
        new, notes = migrate(src)
    with open(path, "w") as f:
        f.write(new)
    if notes:
        sys.stderr.write(f"{path}: " + "; ".join(notes) + "\n")
