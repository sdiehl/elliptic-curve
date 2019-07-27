module Generate.Pretty
  ( prettyBreak
  , prettyDocumentation
  , prettyInline
  , prettyInteger
  , prettyLine
  , prettySection
  , prettyText
  ) where

import Protolude

import Numeric (showHex)
import Text.PrettyPrint.Leijen.Text

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyBreak :: Doc
prettyBreak
  = linebreak

prettyDocumentation :: Doc -> Doc
prettyDocumentation
  = enclose "-- | " dot

prettyInline :: Doc -> Doc
prettyInline
  = braces . enclose "-# INLINE " " #-"

prettyInteger :: Integer -> Doc
prettyInteger
  = (<>) "0x" . pretty . flip showHex ""

prettyLine :: Doc
prettyLine
  = pretty (replicate 79 '-')

prettySection :: Doc -> Doc
prettySection
  = enclose (prettyLine <$$> "-- ") (prettyBreak <> prettyLine <> prettyBreak)

prettyText :: Text -> Doc
prettyText
  = dquotes . pretty
