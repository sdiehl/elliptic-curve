module Generate.Binary.Generate
  ( prettyGenerate
  ) where

import Protolude hiding (toLower)

import Data.Text (toLower)
import Text.PrettyPrint.Leijen.Text

import Generate.Binary.Types
import Generate.Generate
import Generate.Pretty

-------------------------------------------------------------------------------
-- Generate
-------------------------------------------------------------------------------

prettyImports :: Doc
prettyImports
  =    "module Generate.Binary.Parameters"
  <$$> indent 2
    (    "( curves"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> "import Generate.Binary.Types"

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
    prettyCurves' = pretty . toLower . name

prettyParameters :: [Curve] -> Doc
prettyParameters curves
  =    prettySection "Parameters"
  <$$> vcat (punctuate prettyBreak (map prettyParameters' curves))
  where
    prettyParameters' :: Curve -> Doc
    prettyParameters' (Curve name Types{..} Parameters{..})
      =    pretty (toLower name) <+> ":: Curve"
      <$$> pretty (toLower name) <+> "= Curve"
      <$$> indent 2
        (    "{ name =" <+> prettyText name
        <$$> ", types = Types"
        <$$> indent 2
          (    "{ curve =" <+> prettyText curve
          <$$> ", field =" <+> prettyField field
          <$$> "}"
          )
        <$$> ", parameters = Parameters"
        <$$> indent 2
          (    "{ a =" <+> prettyElement a
          <$$> ", b =" <+> prettyElement b
          <$$> ", h =" <+> prettyNatural h
          <$$> ", p =" <+> prettyNatural p
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
