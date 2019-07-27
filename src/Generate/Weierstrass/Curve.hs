module Generate.Weierstrass.Curve
  ( prettyCurve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Curve
import Generate.Weierstrass.Types
import Generate.Pretty

-------------------------------------------------------------------------------
-- Curve
-------------------------------------------------------------------------------

prettyModule :: Types -> Doc
prettyModule Types{..}
  =    "module Curve.Weierstrass." <> pretty curve
  <$$> "  " <> align
    (    "( " <> prettyField field
    <$$> ", P"
    <$$> ", _a"
    <$$> ", _b"
    <$$> ", _g"
    <$$> ", _h"
    <$$> ", _n"
    <$$> ", _p"
    <$$> ", _x"
    <$$> ", _y"
    <$$> ") where"
    )

prettyImports :: Types -> Doc
prettyImports Types{..}
  =    "import Protolude"
  <>   prettyBreak
  <$$> prettyImport field
  <>   prettyBreak
  <$$> "import Curve.Weierstrass (Point(..), WCurve(..), WPoint)"
  <$$> maybe "" pretty imports

prettyTypes :: Types -> Doc
prettyTypes Types{..}
  =    prettySection "Types"
  <$$> prettyDocumentation curve'
  <$$> "data " <> pretty curve
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of " <> curve')
  <$$> prettyType field
  <>   prettyBreak
  <$$> prettyDocumentation (curve' <> " is a Weierstrass curve")
  <$$> "instance WCurve " <> pretty curve <> " " <> prettyField field <> " where"
  <$$> "  " <> align
    (    "a_ = const _a"
    <$$> prettyInline "a_"
    <$$> "b_ = const _b"
    <$$> prettyInline "b_"
    <$$> "g_ = _g"
    <$$> prettyInline "g_"
    <$$> "h_ = const _h"
    <$$> prettyInline "h_"
    <$$> "n_ = const _n"
    <$$> prettyInline "n_"
    <$$> "p_ = const _p"
    <$$> prettyInline "p_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Point of " <> curve')
  <$$> "type P = WPoint " <> pretty curve <> " " <> prettyField field
  where
    curve' :: Doc
    curve' = pretty curve <> " curve"

prettyParameters :: Curve -> Doc
prettyParameters (Curve Types{..} Parameters{..})
  =    prettySection "Parameters"
  <$$> prettyDocumentation ("Coefficient @A@" <> curve')
  <$$> "_a" <> field'
  <$$> "_a = " <> prettyElement a
  <$$> prettyInline "_a"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coefficient @B@" <> curve')
  <$$> "_b" <> field'
  <$$> "_b = " <> prettyElement b
  <$$> prettyInline "_b"
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator" <> curve')
  <$$> "_g :: P"
  <$$> "_g = A _x _y"
  <$$> prettyInline "_g"
  <>   prettyBreak
  <$$> prettyDocumentation ("Cofactor" <> curve')
  <$$> "_h :: Integer"
  <$$> "_h = " <> prettyInteger h
  <$$> prettyInline "_h"
  <>   prettyBreak
  <$$> prettyDocumentation ("Order" <> curve')
  <$$> "_n :: Integer"
  <$$> "_n = " <> prettyInteger n
  <$$> prettyInline "_n"
  <>   prettyBreak
  <$$> prettyDocumentation ("Characteristic" <> curve')
  <$$> "_p :: Integer"
  <$$> "_p = " <> prettyInteger p
  <$$> prettyInline "_p"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @X@" <> curve')
  <$$> "_x" <> field'
  <$$> "_x = " <> prettyElement x
  <$$> prettyInline "_x"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @Y@" <> curve')
  <$$> "_y" <> field'
  <$$> "_y = " <> prettyElement y
  <$$> prettyInline "_y"
  where
    curve' :: Doc
    curve' = " of " <> pretty curve <> " curve"
    field' :: Doc
    field' = " :: " <> prettyField field

prettyCurve :: Curve -> Doc
prettyCurve curve@(Curve types _)
  =    prettyModule types
  <>   prettyBreak
  <$$> prettyImports types
  <>   prettyBreak
  <$$> prettyTypes types
  <>   prettyBreak
  <$$> prettyParameters curve
  <>   prettyBreak
