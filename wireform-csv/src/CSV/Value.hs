-- | Core CSV/TSV data types and configuration.
module CSV.Value (
  CSVDocument (..),
  CSVConfig (..),
  defaultCSV,
  defaultTSV,
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


data CSVDocument = CSVDocument
  { csvHeader :: !(Maybe (Vector Text))
  , csvRows :: !(Vector (Vector Text))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data CSVConfig = CSVConfig
  { csvDelimiter :: !Char
  , csvQuote :: !Char
  , csvEscape :: !Char
  , csvNewline :: !Text
  , csvHasHeader :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


defaultCSV :: CSVConfig
defaultCSV =
  CSVConfig
    { csvDelimiter = ','
    , csvQuote = '"'
    , csvEscape = '"'
    , csvNewline = "\n"
    , csvHasHeader = True
    }


defaultTSV :: CSVConfig
defaultTSV =
  CSVConfig
    { csvDelimiter = '\t'
    , csvQuote = '"'
    , csvEscape = '"'
    , csvNewline = "\n"
    , csvHasHeader = True
    }
