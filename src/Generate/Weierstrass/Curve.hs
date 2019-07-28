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

prettyImports :: Types -> Doc
prettyImports Types{..}
  =    "module Curve.Weierstrass." <> pretty curve
  <$$> "  " <> align
    (    "( Curve(..)"
    <$$> ", " <> prettyField field
    <$$> ", Fr"
    <$$> ", Group(..)"
    <$$> ", P"
    <$$> ", Point(..)"
    <$$> ", WPoint"
    <$$> ", WCurve(..)"
    <$$> ", _a"
    <$$> ", _b"
    <$$> ", _g"
    <$$> ", _h"
    <$$> ", _q"
    <$$> ", _r"
    <$$> ", _x"
    <$$> ", _y"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport field
  <$$> "import Curve.Weierstrass (Point(..), WCurve(..), WPoint)"
  <$$> maybe mempty pretty imports
  <$$> "import Group (Group(..))"

prettyTypes :: Types -> Doc
prettyTypes Types{..}
  =    prettySection "Types"
  <$$> prettyDocumentation curve'
  <$$> "data " <> pretty curve
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of points of " <> curve')
  <$$> prettyType field
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of coefficients of " <> curve')
  <$$> prettyType field'
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
    <$$> "q_ = const _q"
    <$$> prettyInline "q_"
    <$$> "r_ = const _r"
    <$$> prettyInline "r_"
    <$$> "x_ = const _x"
    <$$> prettyInline "x_"
    <$$> "y_ = const _y"
    <$$> prettyInline "y_"
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
  <$$> "_a" <> field''
  <$$> "_a = " <> prettyElement a
  <$$> prettyInline "_a"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coefficient @B@" <> curve')
  <$$> "_b" <> field''
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
  <$$> prettyDocumentation ("Characteristic" <> curve')
  <$$> "_q :: Integer"
  <$$> "_q = " <> prettyInteger q
  <$$> prettyInline "_q"
  <>   prettyBreak
  <$$> prettyDocumentation ("Order" <> curve')
  <$$> "_r :: Integer"
  <$$> "_r = " <> prettyInteger r
  <$$> prettyInline "_r"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @X@" <> curve')
  <$$> "_x" <> field''
  <$$> "_x = " <> prettyElement x
  <$$> prettyInline "_x"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @Y@" <> curve')
  <$$> "_y" <> field''
  <$$> "_y = " <> prettyElement y
  <$$> prettyInline "_y"
  where
    curve' :: Doc
    curve' = " of " <> pretty curve <> " curve"
    field'' :: Doc
    field'' = " :: " <> prettyField field

prettyCurve :: Curve -> Doc
prettyCurve curve@(Curve types _)
  =    prettyImports types
  <>   prettyBreak
  <$$> prettyTypes types
  <>   prettyBreak
  <$$> prettyParameters curve
  <>   prettyBreak
