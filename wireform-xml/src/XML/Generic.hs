{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Generic deriving support for XML serialization.
--
-- Records become elements with field names as child elements:
--
-- @
-- data Person = Person { name :: Text, age :: Int }
--   deriving stock (Generic)
--   deriving anyclass (ToXML, FromXML)
-- -- Produces: \<Person\>\<name\>John\<\/name\>\<age\>30\<\/age\>\<\/Person\>
-- @
--
-- This module re-exports 'ToXML' and 'FromXML' from "XML.Class" which
-- already have Generic default implementations. Import this module for
-- documentation or if you need explicit access to the generic machinery.
module XML.Generic
  ( -- * Re-exports from XML.Class
    ToXML(..)
  , FromXML(..)
  , encodeXML
  , decodeXML
  , GToXML(..)
  , GFromXML(..)
  ) where

import XML.Class
