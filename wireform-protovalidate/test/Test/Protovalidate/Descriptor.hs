{-# LANGUAGE OverloadedStrings #-}

{- | Tests for reading @buf.validate@ rules out of a compiled descriptor: build
a 'FileDescriptorProto' carrying the buf.validate extension (#1159) on field
and message options, then extract and run the rules.
-}
module Test.Protovalidate.Descriptor (tests) where

import CEL (Value (..), celMapFromList)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Proto.Decode (UnknownField (..))
import Proto.Google.Protobuf.Descriptor
import Protovalidate
import Test.Syd


----------------------------------------------------------------------
-- Tiny protobuf wire encoder (to synthesize buf.validate extension bytes)
----------------------------------------------------------------------

varint :: Word64 -> [Word8]
varint n
  | n < 0x80 = [fromIntegral n]
  | otherwise = fromIntegral (n .|. 0x80) : varint (n `shiftR'` 7)
  where
    shiftR' x s = x `div` (2 ^ s)


tag :: Int -> Word64 -> [Word8]
tag fn wt = varint (fromIntegral fn `shiftL` 3 .|. wt)


lenField :: Int -> [Word8] -> [Word8]
lenField fn bs = tag fn 2 ++ varint (fromIntegral (length bs)) ++ bs


varintField :: Int -> Word64 -> [Word8]
varintField fn v = tag fn 0 ++ varint v


strField :: Int -> Text -> [Word8]
strField fn s = lenField fn (BS.unpack (TE.encodeUtf8 s))


-- Constraint { id=1, message=2, expression=3 }
constraint :: Text -> Text -> [Word8]
constraint cid expr = strField 1 cid ++ strField 3 expr


-- FieldConstraints for a string field with min_len=3, required, and one custom CEL.
stringFieldConstraints :: [Word8]
stringFieldConstraints =
  lenField 14 (varintField 2 3) -- string -> StringRules { min_len = 3 }
    ++ varintField 25 1 -- required = true
    ++ lenField 23 (constraint "f.prefix_x" "this.startsWith('x')") -- cel


messageConstraintsBytes :: [Word8]
messageConstraintsBytes = lenField 3 (constraint "m.always" "false") -- cel


----------------------------------------------------------------------
-- Build a descriptor
----------------------------------------------------------------------

userDescriptor :: FileDescriptorProto
userDescriptor =
  defaultFileDescriptorProto
    { fdpName = "user.proto"
    , fdpMessageType = V.singleton userMsg
    }
  where
    userMsg =
      defaultDescriptorProto
        { dpName = "User"
        , dpField = V.singleton nameField
        , dpOptions =
            Just defaultMessageOptions {moUnknownFields = [UnknownLenDelim 1159 (BS.pack messageConstraintsBytes)]}
        }
    nameField =
      defaultFieldDescriptorProto
        { fdpFieldName = "name"
        , fdpFieldNumber = 1
        , fdpFieldType = 9 -- TYPE_STRING
        , fdpFieldOptions =
            Just defaultFieldOptions {fldoUnknownFields = [UnknownLenDelim 1159 (BS.pack stringFieldConstraints)]}
        }


userRules :: MessageRules
userRules = either (error . show) id (messageRulesFromDescriptor userDescriptor "User")


msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])


ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId


tests :: Spec
tests =
  describe
    "descriptor (buf.validate extension #1159)"
    $ sequence_
      [ it "rules are extracted from FieldOptions/MessageOptions" $
          (any ((== "name") . fst) (mrFields userRules)) `shouldBe` True
      , it "standard rule + field CEL + message CEL all fire" $
          ids (validate (msg [("name", VString "ab")]) userRules)
            `shouldBe` sort ["string.min_len", "f.prefix_x", "m.always"]
      , it "a conforming value only trips the always-false message rule" $
          ids (validate (msg [("name", VString "xander")]) userRules)
            `shouldBe` ["m.always"]
      ]
