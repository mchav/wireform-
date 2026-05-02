{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Thrift.Derive.Instances () where

import Thrift.Derive

import Test.Thrift.Derive.Types

deriveThrift ''LogEntry
deriveThrift ''RequestId
deriveThrift ''Severity
deriveThrift ''Event
