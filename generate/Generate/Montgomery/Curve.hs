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
  <$$> indent 2
    (    "( Curve(..)"
    <$$> "," <+> prettyField field
    <$$> ", Fr"
    <$$> ", Group(..)"
    <$$> ", MCurve(..)"
    <$$> ", MPoint"
    <$$> ", MACurve(..)"
    <$$> ", MAPoint"
    <$$> ", PA"
    <$$> ", Point(..)"
    <$$> ", _a"
    <$$> ", _b"
    <$$> ", _h"
    <$$> ", _q"
    <$$> ", _r"
    <$$> ", _x"
    <$$> ", _y"
    <$$> ", gA"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport field
  <>   prettyBreak
  <$$> "import Curve.Montgomery"
  <$$> maybe mempty pretty imports

prettyTypes :: Types -> Doc
prettyTypes Types{..}
  =    prettySection "Types"
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
  <+>  "Fr => MCurve c" <+> pretty curve <+> prettyField field <+> "Fr where"
  <$$> indent 2
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
    <$$> "x_ = const _x"
    <$$> prettyInline "x_"
    <$$> "y_ = const _y"
    <$$> prettyInline "y_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "point")
  <$$> "type PA = MAPoint" <+> pretty curve <+> prettyField field <+> "Fr"
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "is a Montgomery affine curve")
  <$$> "instance MACurve" <+> pretty curve <+> prettyField field <+> "Fr where"
  <$$> indent 2
    (    "gA_ = gA"
    <$$> prettyInline "gA_"
    )
  where
    curve' :: Doc
    curve' = pretty curve <+> "curve"

prettyParameters :: Types -> Parameters -> Doc
prettyParameters Types{..} Parameters{..}
  =    prettySection "Parameters"
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
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @X@ of" <+> curve')
  <$$> "_x ::" <+> prettyField field
  <$$> "_x =" <+> prettyElement x
  <$$> prettyInline "_x"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @Y@ of" <+> curve')
  <$$> "_y ::" <+> prettyField field
  <$$> "_y =" <+> prettyElement y
  <$$> prettyInline "_y"
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator of affine" <+> curve')
  <$$> "gA :: PA"
  <$$> "gA = A _x _y"
  <$$> prettyInline "gA"
  where
    curve' :: Doc
    curve' = pretty curve <+> "curve"

prettyCurve :: Curve -> Doc
prettyCurve (Curve types parameters)
  =    prettyImports types
  <>   prettyBreak
  <$$> prettyTypes types
  <>   prettyBreak
  <$$> prettyParameters types parameters
  <>   prettyBreak
