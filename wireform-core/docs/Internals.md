# Parser Internals

## The Magic Ring Buffer

A buffer of size N (power of two, page-aligned) backed by a single
anonymous shared memory object mapped twice into a contiguous 2N-byte
virtual region. Bytes at `base + N + k` are physically the same as
`base + k`.

This means any read of up to N bytes starting anywhere in `[base, base + N)`
is contiguous in virtual memory. The parser does straight-line pointer
bumping with no wrap logic — the MMU handles it.

Implementation: `memfd_create` on Linux, `shm_open` on POSIX,
`VirtualAlloc2` with placeholders on Windows.

## Parser Type

```haskell
newtype Parser e a = Parser
  { runParser# :: forall r.
                  PromptTag# (Step e r)
               -> ParserEnv
               -> Addr#     -- eob (end of buffer)
               -> Addr#     -- cur (current position)
               -> State# RealWorld
               -> (# State# RealWorld, Res# e a #)
  }
```

The representation mirrors flatparse exactly: raw `Addr#` pointers,
`State# RealWorld` threading, unboxed sum results. The extra
`PromptTag#` and `ParserEnv` parameters support streaming suspension
but are dead weight for whole-input parsing.

## Unboxed Result Sum

```haskell
type Res# e a = (# (# a, Addr# #) | (# #) | (# e #) #)
pattern OK# a s   -- success + new position
pattern Fail#     -- recoverable failure
pattern Err# e    -- unrecoverable error
```

On `Fail#` and `Err#` branches, `unsafeCoerce#` avoids reconstructing
the sum at different type parameters — identical to flatparse's approach.

## The Suspend Mechanism

When a primitive's bounds check fails (needs N bytes, has fewer),
`ensureNSlow` fires:

1. `control0# tag handler` captures the parser stack up to the `prompt#` frame
2. The handler builds a `StepSuspend` with a `Resume` continuation
3. The driver receives `StepSuspend`, calls `transportWaitData`
4. When data arrives, the driver calls `resumeContinue newCur newEnd`
5. This re-establishes a `prompt#` frame and invokes the captured continuation
6. The parser resumes from the `ensureNSlow` call site with updated pointers

The fast path (enough bytes available) is a single `<=# minusAddr#`
comparison — no memory access to the mutable end pointer, no IO, no
allocation.

## Zero-Copy ByteString Slicing

`takeBs` and `byteStringOf` create `ForeignPtr`-backed `ByteString`
values that reference the input memory directly:

- For `parseByteString`: slices of the input `ByteString`
- For ring-backed streaming: slices of the mmap'd ring memory

The `peBackingFp` field in `ParserEnv` carries the `ForeignPtrContents`
that keeps the backing memory alive.

## EOF Classification

The driver uses a high-water mark to distinguish clean EOF (stream
ended between messages) from unexpected EOF (stream ended mid-message).
If the parser never observed any bytes before EOF, it's clean EOF.
Otherwise it's unexpected EOF.
