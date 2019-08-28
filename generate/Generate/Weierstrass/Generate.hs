module Generate.Weierstrass.Generate
  ( prettyGenerate
  ) where

import Protolude

import Data.Text (toLower)
import Text.PrettyPrint.Leijen.Text

import Generate.Generate
import Generate.Pretty
import Generate.Weierstrass.Types

-------------------------------------------------------------------------------
-- Generate
-------------------------------------------------------------------------------

prettyImports :: Doc
prettyImports
  =    "module Generate.Weierstrass.Parameters"
  <$$> indent 2
    (    "( curves"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> "import Protolude"
  <>   prettyBreak
  <$$> "import Generate.Weierstrass.Types"

prettyCurves :: [Curve] -> Doc
prettyCurves curves
  =    prettySection "Curves"
  <$$> "curves :: [Curve]"
  <$$> "curves ="
  <$$> indent 2
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
      =    pretty (toLower curve) <+> ":: Curve"
      <$$> pretty (toLower curve) <+> "= Curve"
      <$$> indent 2
        (    "{ types = Types"
        <$$> indent 2
          (    "{ curve   =" <+> prettyText curve
          <$$> ", field   =" <+> prettyField field
          <$$> ", field'  =" <+> prettyField field'
          <$$> ", imports = "
          <>   maybe "Nothing" ((<>) "Just " . prettyText) imports
          <$$> "}"
          )
        <$$> ", parameters = Parameters"
        <$$> indent 2
          (    "{ a =" <+> prettyElement a
          <$$> ", b =" <+> prettyElement b
          <$$> ", h =" <+> prettyNatural h
          <$$> ", q =" <+> prettyNatural q
          <$$> ", r =" <+> prettyNatural r
          <$$> ", x =" <+> prettyElement x
          <$$> ", y =" <+> prettyElement y
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
