module Generate.Montgomery.Curve
  ( prettyCurve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Curve
import Generate.Montgomery.Types
import Generate.Pretty

-------------------------------------------------------------------------------
-- Curve
-------------------------------------------------------------------------------

prettyImports :: Types -> Doc
prettyImports Types{..}
  =    "module Curve.Montgomery." <> pretty curve
  <$$> " " <+> align
    (    "( AP"
    <$$> ", Curve(..)"
    <$$> "," <+> prettyField field
    <$$> ", Fr"
    <$$> ", Group(..)"
    <$$> ", MCurve(..)"
    <$$> ", MPoint"
    <$$> ", MACurve(..)"
    <$$> ", MAPoint"
    <$$> ", Point(..)"
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
  <$$> "import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))"
  <$$> maybe mempty pretty imports
  <$$> "import Group (Group(..))"

prettyForm :: Curve -> Doc
prettyForm (Curve Types{..} Parameters{..})
  =    prettySection curve'
  <$$> prettyDocumentation curve'
  <$$> "data" <+> pretty curve
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of points of" <+> curve')
  <$$> prettyType field
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of coefficients of" <+> curve')
  <$$> prettyType field'
  <>   prettyBreak
  <$$> prettyDocumentation (curve' <+> "is a Montgomery curve")
  <$$> "instance Curve 'Montgomery c" <+> pretty curve <+> prettyField field
  <+>  "=> MCurve c" <+> pretty curve <+> prettyField field <+> "where"
  <$$> " " <+> align
    (    "a_ = const _a"
    <$$> prettyInline "a_"
    <$$> "b_ = const _b"
    <$$> prettyInline "b_"
    <$$> "h_ = const _h"
    <$$> prettyInline "h_"
    <$$> "q_ = const _q"
    <$$> prettyInline "q_"
    <$$> "r_ = const _r"
    <$$> prettyInline "r_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Coefficient @A@ of" <+> curve')
  <$$> "_a ::" <+> prettyField field
  <$$> "_a =" <+> prettyElement a
  <$$> prettyInline "_a"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coefficient @B@ of" <+> curve')
  <$$> "_b ::" <+> prettyField field
  <$$> "_b =" <+> prettyElement b
  <$$> prettyInline "_b"
  <>   prettyBreak
  <$$> prettyDocumentation ("Cofactor of" <+> curve')
  <$$> "_h :: Integer"
  <$$> "_h =" <+> prettyInteger h
  <$$> prettyInline "_h"
  <>   prettyBreak
  <$$> prettyDocumentation ("Characteristic of" <+> curve')
  <$$> "_q :: Integer"
  <$$> "_q =" <+> prettyInteger q
  <$$> prettyInline "_q"
  <>   prettyBreak
  <$$> prettyDocumentation ("Order of" <+> curve')
  <$$> "_r :: Integer"
  <$$> "_r =" <+> prettyInteger r
  <$$> prettyInline "_r"
  where
    curve' :: Doc
    curve' = pretty curve <+> "curve"

prettyAffine :: Curve -> Doc
prettyAffine (Curve Types{..} Parameters{..})
  =    prettySection "Affine coordinates"
  <$$> prettyDocumentation ("Affine" <+> pretty curve <+> "point")
  <$$> "type AP = MAPoint" <+> pretty curve <+> prettyField field
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "is a Montgomery affine curve")
  <$$> "instance MACurve" <+> pretty curve <+> prettyField field <+> "where"
  <$$> " " <+> align
    (    "g_ = _g"
    <$$> prettyInline "g_"
    <$$> "x_ = const _x"
    <$$> prettyInline "x_"
    <$$> "y_ = const _y"
    <$$> prettyInline "y_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator of affine" <+> curve')
  <$$> "_g :: AP"
  <$$> "_g = A _x _y"
  <$$> prettyInline "_g"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @X@ of affine" <+> curve')
  <$$> "_x ::" <+> prettyField field
  <$$> "_x =" <+> prettyElement x
  <$$> prettyInline "_x"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @Y@ of affine" <+> curve')
  <$$> "_y ::" <+> prettyField field
  <$$> "_y =" <+> prettyElement y
  <$$> prettyInline "_y"
  where
    curve' :: Doc
    curve' = pretty curve <+> "curve"

prettyCurve :: Curve -> Doc
prettyCurve curve@(Curve types _)
  =    prettyImports types
  <>   prettyBreak
  <$$> prettyForm curve
  <>   prettyBreak
  <$$> prettyAffine curve
  <>   prettyBreak
