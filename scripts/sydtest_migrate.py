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
    # Only treat the suite as hedgehog-based if it actually imports
    # hedgehog. `testProperty` is ambiguous (tasty-quickcheck uses it
    # too), so keying on it would wrongly pull in sydtest-hedgehog.
    saw_hedgehog = "Test.Tasty.Hedgehog" in src or re.search(
        r"(?m)^import\s+(qualified\s+)?Hedgehog\b", src
    ) is not None
    src = _fix_imports(src)

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
    # HUnit's `Assertion` alias (IO ()) is gone with the tasty imports.
    src = _replace_ident(src, "Assertion", "IO ()")

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
    src, nAE = _convert_assert_equal(src)
    notes += nAE
    src, nAP = _convert_assert_pred(src)
    notes += nAP
    # tasty's testCase forced the body to IO (); sydtest's `it` accepts
    # many IsTest instances, so a polymorphic `fail`/`pure ()` body is
    # ambiguous. expectationFailure :: String -> IO a pins it to IO.
    src = _replace_ident(src, "fail", "expectationFailure")
    src, n3 = _convert_assert_bool(src)
    notes += n3
    if "assertEqual" in src:
        notes.append("assertEqual remained (manual check)")

    out, notes2 = _wrap_describe_lists(src)
    notes += notes2

    for s in SUSPECT:
        if s in out:
            notes.append(f"contains {s!r}")
    return out, notes


# tasty sub-libraries that sydtest fully subsumes (just drop the import).
_DROP_SUBMODULES = {
    "HUnit", "Hedgehog", "SmallCheck",
    "Ingredients", "Runners", "Options", "Providers",
}


def _fix_imports(src: str) -> str:
    """Rewrite `import Test.Tasty[.Sub] [...]` statements, consuming any
    multi-line parenthesised import list. Plain Test.Tasty becomes
    Test.Syd; known sub-libraries are dropped (covered by sydtest)."""
    pat = re.compile(
        r"^[ \t]*import[ \t]+(?:qualified[ \t]+)?Test\.Tasty(\.[A-Za-z0-9_.]+)?",
        re.M,
    )
    repls: list[tuple[int, int, str]] = []
    needs_syd = False
    has_syd_already = re.search(r"^[ \t]*import[ \t]+Test\.Syd\b", src, re.M) is not None
    for m in pat.finditer(src):
        start = m.start()
        end = m.end()
        # optional `as Name`
        rest = src[end:]
        am = re.match(r"[ \t]+as[ \t]+[A-Za-z0-9_.]+", rest)
        if am:
            end += am.end()
        # optional (possibly multi-line) explicit import/hiding list
        j = end
        # allow `hiding` keyword
        hm = re.match(r"[ \t]+hiding\b", src[j:])
        if hm:
            j += hm.end()
        k = j
        while k < len(src) and src[k] in " \t\n":
            k += 1
        if k < len(src) and src[k] == "(":
            close = _match_balanced(src, k)
            if close != -1:
                end = close
        sub = m.group(1)
        if sub is None:
            repls.append((start, end, "import Test.Syd"))
            needs_syd = True
        else:
            name = sub.lstrip(".").split(".")[0]
            if name == "QuickCheck":
                # tasty-quickcheck re-exports QuickCheck's Gen/Arbitrary/
                # Property/NonNegative/==> etc., which the tests rely on.
                # sydtest runs QuickCheck properties natively, so swap to
                # the underlying Test.QuickCheck (deduped below).
                repls.append((start, end, "import Test.QuickCheck"))
            elif name in _DROP_SUBMODULES:
                repls.append((start, end, ""))  # drop, possibly leaving blank line
            else:
                repls.append((start, end, "import Test.Syd"))
                needs_syd = True

    for start, end, text in reversed(repls):
        # if dropping, also swallow a trailing newline to avoid blank lines
        if text == "" and end < len(src) and src[end] == "\n":
            end += 1
        src = src[:start] + text + src[end:]

    # collapse duplicate `import Test.Syd` lines (keep first)
    seen = False
    out_lines = []
    for ln in src.splitlines(keepends=True):
        if re.match(r"^[ \t]*import[ \t]+Test\.Syd[ \t]*(\n|$)", ln):
            if seen:
                continue
            seen = True
        out_lines.append(ln)
    src = "".join(out_lines)

    # collapse duplicate bare `import Test.QuickCheck` lines (keep first);
    # a pre-existing qualified/explicit import is left as-is.
    seen_qc = False
    out_lines = []
    for ln in src.splitlines(keepends=True):
        if re.match(r"^[ \t]*import[ \t]+Test\.QuickCheck[ \t]*(\n|$)", ln):
            if seen_qc:
                continue
            seen_qc = True
        out_lines.append(ln)
    src = "".join(out_lines)

    if needs_syd and not seen and not has_syd_already:
        # No surviving Test.Syd import; add one after the last import.
        lines = src.splitlines(keepends=True)
        last_imp = -1
        for i, ln in enumerate(lines):
            if ln.startswith("import "):
                last_imp = i
        ins = last_imp + 1 if last_imp >= 0 else 0
        lines.insert(ins, "import Test.Syd\n")
        src = "".join(lines)
    return src


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


def _convert_assert_pred(src: str) -> tuple[str, list[str]]:
    r"""HUnit's `lhs @? msg` (assert lhs is True with message) ->
    `lhs `shouldBe` True`. The message is dropped. Run this AFTER `@?=`
    has already been rewritten so only the bare `@?` operator remains."""
    notes: list[str] = []
    out = []
    i = 0
    while True:
        j = src.find("@?", i)
        if j == -1:
            out.append(src[i:])
            break
        # `@?=` was already handled; guard anyway.
        if j + 2 < len(src) and src[j + 2] == "=":
            out.append(src[i : j + 3])
            i = j + 3
            continue
        out.append(src[i:j])
        k = _skip_ws(src, j + 2)
        msg, k = _read_arg(src, k)
        if msg is None:
            notes.append("@? without parseable message; left as-is")
            out.append(src[j : j + 2])
            i = j + 2
            continue
        out.append("`shouldBe` True")
        i = k
    return "".join(out), notes


def _convert_assert_equal(src: str) -> tuple[str, list[str]]:
    r"""assertEqual MSG EXPECTED ACTUAL  ->  ACTUAL `shouldBe` EXPECTED."""
    notes: list[str] = []
    out = []
    i = 0
    needle = "assertEqual"
    while True:
        j = src.find(needle, i)
        if j == -1:
            out.append(src[i:])
            break
        prev = src[j - 1] if j > 0 else " "
        if prev.isalnum() or prev == "_" or prev == "'":
            out.append(src[i : j + len(needle)])
            i = j + len(needle)
            continue
        out.append(src[i:j])
        k = _skip_ws(src, j + len(needle))
        msg, k = _read_arg(src, k)
        e1 = _skip_ws(src, k)
        expected, k2 = _read_arg(src, e1)
        e2 = _skip_ws(src, k2)
        actual, k3 = _read_arg(src, e2)
        if msg is None or expected is None or actual is None:
            notes.append("assertEqual: could not parse args; skipped")
            out.append(needle)
            i = j + len(needle)
            continue
        out.append(f"{actual} `shouldBe` {expected}")
        notes.append("converted assertEqual -> shouldBe (verify arg shapes)")
        i = k3
    return "".join(out), notes


def _skip_ws(src: str, i: int) -> int:
    while i < len(src) and src[i] in " \t\n":
        i += 1
    return i


def _read_arg(src: str, i: int):
    """Read one Haskell expression argument starting at i. Supports a
    parenthesised group, a string literal, or a bare token. Returns
    (text, end) or (None, i)."""
    if i >= len(src):
        return None, i
    c = src[i]
    if c == "(":
        end = _match_balanced(src, i)
        return (src[i:end], end) if end != -1 else (None, i)
    if c == '"':
        end = _read_string_literal(src, i)
        return (src[i:end], end) if end != -1 else (None, i)
    # bare token: identifier / qualified name
    m = re.match(r"[A-Za-z0-9_.']+", src[i:])
    if m:
        return src[i : i + m.end()], i + m.end()
    return None, i


def _is_string_literal(tok: str) -> bool:
    return tok.startswith('"') and tok.endswith('"')


def _convert_assert_bool(src: str) -> tuple[str, list[str]]:
    r"""assertBool MSG COND  ->  COND `shouldBe` True (literal message,
    carried by the `it` label) or a message-preserving
    `if COND then pure () else expectationFailure MSG`."""
    notes: list[str] = []
    out = []
    i = 0
    needle = "assertBool"
    while True:
        j = src.find(needle, i)
        if j == -1:
            out.append(src[i:])
            break
        prev = src[j - 1] if j > 0 else " "
        if prev.isalnum() or prev == "_" or prev == "'":
            out.append(src[i : j + len(needle)])
            i = j + len(needle)
            continue
        out.append(src[i:j])
        k = _skip_ws(src, j + len(needle))
        msg, k = _read_arg(src, k)
        if msg is None:
            notes.append("assertBool: could not parse message; skipped")
            out.append(src[j:k] or needle)
            i = max(k, j + len(needle))
            continue
        e = _skip_ws(src, k)
        cond, eend = _read_arg(src, e)
        if cond is None:
            notes.append("assertBool: could not parse condition; skipped")
            out.append(src[j:e])
            i = e
            continue
        cond_txt = cond if cond.startswith("(") else f"({cond})"
        if _is_string_literal(msg):
            out.append(f"{cond_txt} `shouldBe` True")
        else:
            msg_txt = msg if msg.startswith("(") else f"({msg})"
            out.append(
                f"(if {cond_txt} then pure () else expectationFailure {msg_txt})"
            )
        i = eend
    return "".join(out), notes


def _wrap_describe_lists(src: str) -> tuple[str, list[str]]:
    """Insert `$ sequence_` between a `describe "..."` and a following `[`.

    Handles both `describe "x"\n  [ ... ]` and `describe "x" [ ... ]`.
    Leaves `describe "x" $ do` untouched.
    """
    notes: list[str] = []
    # Match `describe <name>` where <name> is a string literal or a bare
    # token, then — skipping whitespace and -- line comments — a '['.
    # Insert `$ sequence_` right after the name so the existing list is
    # consumed as a do-block-equivalent.
    name = r'"(?:[^"\\]|\\.)*"|[^\s\[]+'
    gap = r'(?:[ \t]*\n|[ \t]+|[ \t]*--[^\n]*\n)*'
    pattern = re.compile(rf'(\bdescribe{gap}(?:{name}))({gap})\[')

    def repl(m: re.Match) -> str:
        return f"{m.group(1)} $ sequence_{m.group(2)}["

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
