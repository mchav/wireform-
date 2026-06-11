{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MsgPack.Derive.Instances () where

import MsgPack.Derive
import Test.MsgPack.Derive.Types


deriveMsgPack ''Profile
deriveMsgPack ''Tag
deriveMsgPack ''Color
deriveMsgPack ''Shape
