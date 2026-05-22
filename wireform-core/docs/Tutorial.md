# wireform-core Parser Tutorial

## Quick Start

```haskell
import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)

-- Parse a 32-bit big-endian length prefix followed by the payload
lengthPrefixed :: Parser String ByteString
lengthPrefixed = do
  len <- anyWord32be
  takeBs (fromIntegral len)

main :: IO ()
main = case parseByteString lengthPrefixed "\x00\x00\x00\x05hello" of
  Right bs -> putStrLn ("Got: " <> show bs)
  Left err -> putStrLn ("Parse error: " <> show err)
```

## Example 1: Simple Binary Protocol

A protocol with a 1-byte tag, then type-specific payload:

```haskell
data Message
  = Ping
  | Data ByteString
  | Quit

parseMessage :: Parser String Message
parseMessage = do
  tag <- anyWord8
  case tag of
    0x01 -> pure Ping
    0x02 -> do
      len <- anyWord16be
      payload <- takeBs (fromIntegral len)
      pure (Data payload)
    0x03 -> pure Quit
    _    -> err ("unknown tag: " <> show tag)
```

## Example 2: Text-based Protocol (Redis RESP)

```haskell
respSimpleString :: Parser String ByteString
respSimpleString = do
  word8 0x2B  -- '+'
  s <- byteStringOf (skipMany (skipSatisfyAscii (\c -> c /= '\r' && c /= '\n')))
  word8 0x0D  -- '\r'
  word8 0x0A  -- '\n'
  pure s

respInteger :: Parser String Int
respInteger = do
  word8 0x3A  -- ':'
  n <- anyAsciiDecimalInt
  word8 0x0D >> word8 0x0A
  pure n
```

## Example 3: Streaming from a Socket

```haskell
import Wireform.Network
import Wireform.Parser
import Wireform.Parser.Driver

server :: Socket -> IO ()
server sock =
  withRecvTransport (profileConfig Throughput) sock $ \t ->
    runParserLoop t parseMessage $ \msg -> do
      case msg of
        Quit -> pure Stop
        _    -> handleMessage msg >> pure Continue
```

## Using `cut` for Better Error Messages

```haskell
parsePacket :: Parser String Packet
parsePacket = do
  magic <- anyWord32be
  cut (guard (magic == 0xDEADBEEF)) "invalid magic number"
  -- After cut, any subsequent failure becomes an unrecoverable error
  -- with the message above, rather than a silent backtrack
  len <- anyWord16be
  payload <- takeBs (fromIntegral len)
  pure (Packet payload)
```

## CPS Primitives (Advanced)

For maximum performance, avoid boxing intermediate values:

```haskell
-- Instead of:
parseTagged = do
  tag <- anyWord8  -- boxes the Word8
  case tag of ...

-- Use CPS:
parseTagged = withAnyWord8 $ \tag ->
  case tag of ...
```

## Adapting Existing Parsers

If you have an attoparsec parser:

```haskell
import Wireform.Parser.Adapter.Attoparsec (fromAttoparsec)
import Wireform.Network

main = withRecvTransport cfg sock $ \t ->
  runChunked t ChunkCopy (fromAttoparsec myAttoparsecParser)
```
