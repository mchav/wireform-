-- | Iceberg-flavoured re-export of "Wireform.Hash". The C/SIMDe kernels
-- now live in @wireform-core@ so that @wireform-parquet@ and other format
-- packages can share them without depending on @wireform-iceberg@.
--
-- This module is kept around as a stable entry point for historical
-- callers and for the iceberg-test / iceberg-bench suites; new code can
-- import "Wireform.Hash" directly.
module Iceberg.SIMD
  ( -- * Murmur3 32-bit (Iceberg @BucketUtil@)
    murmur3_32
  , bucketLong
    -- * XXH64
  , xxh64
    -- * Roaring 32-bit container
  , roaringDecodeArray
  , roaringDecodeBitset
  , roaringContains
  , roaringEncodeArray
  , roaringEncodeBitset
  , RoaringContainerKind(..)
  ) where

import Wireform.Hash
