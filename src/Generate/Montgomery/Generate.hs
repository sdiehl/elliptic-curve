module Generate.Montgomery.Generate
  ( prettyGenerate
  ) where

import Protolude

import Data.Text (toLower)
import Text.PrettyPrint.Leijen.Text

import Generate.Generate
import Generate.Montgomery.Types
import Generate.Pretty

-------------------------------------------------------------------------------
-- Generate
-------------------------------------------------------------------------------

prettyImports :: Doc
prettyImports
  =    "module Generate.Montgomery.Parameters"
  <$$> "  " <> align
    (    "( curves"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> "import Protolude"
  <>   prettyBreak
  <$$> "import Generate.Montgomery.Types"

prettyCurves :: [Curve] -> Doc
prettyCurves curves
  =    prettySection "Curves"
  <$$> "curves :: [Curve]"
  <$$> "curves ="
  <$$> "  " <> align
    (    "[ "
    <>   hcat (punctuate "\n, " (map prettyCurves' curves))
    <$$> "]"
    )
  where
    prettyCurves' :: Curve -> Doc
    prettyCurves' = pretty . toLower . curve . types

prettyParameters :: [Curve] -> Doc
prettyParameters curves
  =    prettySection "Parameters"
  <$$> vcat (punctuate prettyBreak (map prettyParameters' curves))
  where
    prettyParameters' :: Curve -> Doc
    prettyParameters' (Curve Types{..} Parameters{..})
      =    pretty (toLower curve) <> " :: Curve"
      <$$> pretty (toLower curve) <> " = Curve"
      <$$> "  " <> align
        (    "{ types = Types"
        <$$> "  " <> align
          (    "{ curve   = " <> prettyText curve <> ""
          <$$> ", field   = " <> prettyField field
          <$$> ", imports = "
          <>   maybe "Nothing" ((<>) "Just " . prettyText) imports
          <$$> "}"
          )
        <$$> ", parameters = Parameters"
        <$$> "  " <> align
          (    "{ a = " <> prettyElement a
          <$$> ", b = " <> prettyElement b
          <$$> ", x = " <> prettyElement x
          <$$> ", y = " <> prettyElement y
          <$$> ", h = " <> prettyInteger h
          <$$> ", n = " <> prettyInteger n
          <$$> ", p = " <> prettyInteger p
          <$$> "}"
          )
        <$$> "}"
        )

prettyGenerate :: [Curve] -> Doc
prettyGenerate curves
  =    prettyImports
  <>   prettyBreak
  <$$> prettyCurves curves
  <>   prettyBreak
  <$$> prettyParameters curves
  <>   prettyBreak
