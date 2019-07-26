module Generate.Pretty
  ( prettyBreak
  , prettyDocumentation
  , prettyInline
  , prettyInteger
  , prettyLine
  , prettySection
  ) where

import Protolude

import Numeric (showHex)
import Text.PrettyPrint.Leijen.Text

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyBreak :: Doc
prettyBreak
  = "\n\n"

prettyDocumentation :: Doc -> Doc
prettyDocumentation
  = (<>) "-- | "

prettyInline :: Doc -> Doc
prettyInline inline
  = "{-# INLINE " <> inline <> " #-}"

prettyInteger :: Integer -> Doc
prettyInteger
  = (<>) "0x" . pretty . flip showHex ""

prettyLine :: Doc
prettyLine
  = pretty (replicate 79 '-')

prettySection :: Doc -> Doc
prettySection section
  =    prettyLine
  <$$> "-- " <> section
  <$$> prettyLine
  <>   prettyBreak
