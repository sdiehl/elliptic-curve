module Generate.Generate
  ( prettyGenerate
  ) where

import Protolude

import Data.Text (toLower)
import Text.PrettyPrint.Leijen.Text (Doc, (<$$>), align, hcat, pretty,
                                     punctuate, vcat)

import Generate.Type (Curve(..), Element(..))

-------------------------------------------------------------------------------
-- Elements
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (BF n ) = "BF " <> pretty n
prettyElement (EF ns) = "EF " <> align
  (    (if null ns then "[" else "[ ")
  <>   hcat (punctuate "\n, " (map prettyElement ns))
  <$$> "]"
  )
prettyElement (PF n ) = "PF " <> pretty n

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

prettyCurve :: Curve -> Doc
prettyCurve Curve{..} = pretty (toLower i) <> " :: Curve"
  <$$> pretty (toLower i) <> " = Curve"
  <$$> "  " <> align
    (    "{ i = \"" <> pretty i <> "\""
    <$$> ", a = " <> prettyElement a
    <$$> ", b = " <> prettyElement b
    <$$> ", x = " <> prettyElement x
    <$$> ", y = " <> prettyElement y
    <$$> ", h = " <> pretty h
    <$$> ", n = " <> pretty n
    <$$> ", p = " <> pretty p
    <$$> "}"
    )

-------------------------------------------------------------------------------
-- Generator
-------------------------------------------------------------------------------

prettyBreak :: Doc
prettyBreak = "\n\n"

prettyLine :: Doc
prettyLine = pretty (replicate 79 '-')

prettyModule :: Doc -> Doc -> Doc
prettyModule form form'
  =    "module Generate.Curve." <> pretty form
  <$$> "  " <> align
    (    "( " <> form'
    <$$> ") where"
    )

prettyImports :: Doc
prettyImports = "import Generate.Type (Curve(..), Element(..))"

prettyCurves :: Doc -> [Curve] -> Doc
prettyCurves form' curves
  =    prettyLine
  <$$> "-- Curves"
  <$$> prettyLine
  <>   prettyBreak
  <$$> form' <> " :: [Curve]"
  <$$> form' <> " ="
  <$$> "  " <> align
    (    "[ " <> hcat (punctuate "\n, " (map (pretty . toLower . i) curves))
    <$$> "]"
    )

prettyParameters :: [Curve] -> Doc
prettyParameters curves
  =    prettyLine
  <$$> "-- Parameters"
  <$$> prettyLine
  <>   prettyBreak
  <$$> vcat (punctuate prettyBreak (map prettyCurve curves))
  <>   prettyBreak

prettyGenerate :: Text -> [Curve] -> Doc
prettyGenerate form curves
  =    prettyModule (pretty form) form'
  <>   prettyBreak
  <$$> prettyImports
  <>   prettyBreak
  <$$> prettyCurves form' curves
  <>   prettyBreak
  <$$> prettyParameters curves
  where
    form' :: Doc
    form' = pretty (toLower form) <> "Curves"
