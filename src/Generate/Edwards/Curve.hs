module Generate.Edwards.Curve
  ( prettyCurve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Curve
import Generate.Edwards.Types
import Generate.Pretty

-------------------------------------------------------------------------------
-- Curve
-------------------------------------------------------------------------------

prettyImports :: Types -> Doc
prettyImports Types{..}
  =    "module Curve.Edwards." <> pretty curve
  <$$> " " <+> align
    (    "( AP"
    <$$> ", Curve(..)"
    <$$> ", ECurve(..)"
    <$$> ", EPoint"
    <$$> ", EACurve(..)"
    <$$> ", EAPoint"
    <$$> "," <+> prettyField field
    <$$> ", Fr"
    <$$> ", Group(..)"
    <$$> ", Point(..)"
    <$$> ", _a"
    <$$> ", _d"
    <$$> ", _h"
    <$$> ", _q"
    <$$> ", _r"
    <$$> ", gA"
    <$$> ", xA"
    <$$> ", yA"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport field
  <$$> "import Curve.Edwards (ECurve(..), EPoint, EACurve(..), EAPoint, Point(..))"
  <$$> maybe mempty pretty imports
  <$$> "import Group (Group(..))"

prettyForm :: Types -> Parameters -> Doc
prettyForm Types{..} Parameters{..}
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
  <$$> prettyDocumentation (curve' <+> "is an Edwards curve")
  <$$> "instance Curve 'Edwards c" <+> pretty curve <+> prettyField field
  <+>  "=> ECurve c" <+> pretty curve <+> prettyField field <+> "where"
  <$$> " " <+> align
    (    "a_ = const _a"
    <$$> prettyInline "a_"
    <$$> "d_ = const _d"
    <$$> prettyInline "d_"
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
  <$$> prettyDocumentation ("Coefficient @D@ of" <+> curve')
  <$$> "_d ::" <+> prettyField field
  <$$> "_d =" <+> prettyElement d
  <$$> prettyInline "_d"
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

prettyAffine :: Types -> Affine -> Doc
prettyAffine Types{..} Affine{..}
  =    prettySection "Affine coordinates"
  <$$> prettyDocumentation ("Affine" <+> pretty curve <+> "point")
  <$$> "type AP = EAPoint" <+> pretty curve <+> prettyField field
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "is an Edwards affine curve")
  <$$> "instance EACurve" <+> pretty curve <+> prettyField field <+> "where"
  <$$> " " <+> align
    (    "gA_ = gA"
    <$$> prettyInline "gA_"
    <$$> "xA_ = const xA"
    <$$> prettyInline "xA_"
    <$$> "yA_ = const yA"
    <$$> prettyInline "yA_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator of affine" <+> curve')
  <$$> "gA :: AP"
  <$$> "gA = A xA yA"
  <$$> prettyInline "gA"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @X@ of affine" <+> curve')
  <$$> "xA ::" <+> prettyField field
  <$$> "xA =" <+> prettyElement xA
  <$$> prettyInline "xA"
  <>   prettyBreak
  <$$> prettyDocumentation ("Coordinate @Y@ of affine" <+> curve')
  <$$> "yA ::" <+> prettyField field
  <$$> "yA =" <+> prettyElement yA
  <$$> prettyInline "yA"
  where
    curve' :: Doc
    curve' = pretty curve <+> "curve"

prettyCurve :: Curve -> Doc
prettyCurve (Curve types parameters affine)
  =    prettyImports types
  <>   prettyBreak
  <$$> prettyForm types parameters
  <>   prettyBreak
  <$$> prettyAffine types affine
  <>   prettyBreak
