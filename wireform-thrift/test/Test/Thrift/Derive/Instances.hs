{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Thrift.Derive.Instances () where

import Test.Thrift.Derive.Types
import Thrift.Derive


deriveThrift ''LogEntry
deriveThrift ''RequestId
deriveThrift ''Severity
deriveThrift ''Event
