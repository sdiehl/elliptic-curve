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
  =    "module Data.Curve.Edwards." <> pretty curve
  <$$> indent 2
    (    "( module Data.Curve.Edwards"
    <$$> "-- *" <+> pretty curve <+> "curve"
    <$$> ", module Data.Curve.Edwards." <> pretty curve
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport
  <>   prettyBreak
  <$$> "import Data.Curve.Edwards"
  <$$> "import Data.Curve.Edwards.Base (ECurve(..), EACurve(..), EPCurve(..))"
  <$$> maybe mempty pretty imports

prettyTypes :: Types -> Doc
prettyTypes Types{..}
  =    prettySection "Types"
  <$$>
    ( if base == curve
      then
        (    prettyDocumentation curve'
        <$$> "data" <+> pretty base
        <>   prettyBreak
        )
      else mempty
    )
  <$$> prettyDocumentation ("Field of points of" <+> curve')
  <$$> prettyType field "Q"
  <>   prettyBreak
  <$$> prettyDocumentation ("Field of coefficients of" <+> curve')
  <$$> prettyType field' "R"
  <>   prettyBreak
  <$$> prettyComment (curve' <+> "is an Edwards curve")
  <$$> "instance Curve 'Edwards c" <+> pretty base <+> prettyField field
  <+>  "Fr => ECurve c" <+> pretty base <+> prettyField field <+> "Fr where"
  <$$> indent 2
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
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "point")
  <$$> "type PA = EAPoint" <+> pretty base <+> prettyField field <+> "Fr"
  <>   prettyBreak
  <$$> prettyComment ("Affine" <+> curve' <+> "is an Edwards affine curve")
  <$$> "instance EACurve" <+> pretty base <+> prettyField field <+> "Fr where"
  <$$> indent 2
    (    "gA_ = gA"
    <$$> prettyInline "gA_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Projective" <+> pretty curve <+> "point")
  <$$> "type PP = EPPoint" <+> pretty base <+> prettyField field <+> "Fr"
  <>   prettyBreak
  <$$> prettyComment ("Projective" <+> curve' <+> "is an Edwards projective curve")
  <$$> "instance EPCurve" <+> pretty base <+> prettyField field <+> "Fr where"
  <$$> indent 2
    (    "gP_ = gP"
    <$$> prettyInline "gP_"
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
  <$$> prettyDocumentation ("Coefficient @D@ of" <+> curve')
  <$$> "_d ::" <+> prettyField field
  <$$> "_d =" <+> prettyElement d
  <$$> prettyInline "_d"
  <>   prettyBreak
  <$$> prettyDocumentation ("Cofactor of" <+> curve')
  <$$> "_h :: Natural"
  <$$> "_h =" <+> prettyNatural h
  <$$> prettyInline "_h"
  <>   prettyBreak
  <$$> prettyDocumentation ("Characteristic of" <+> curve')
  <$$> "_q :: Natural"
  <$$> "_q =" <+> prettyNatural q
  <$$> prettyInline "_q"
  <>   prettyBreak
  <$$> prettyDocumentation ("Order of" <+> curve')
  <$$> "_r :: Natural"
  <$$> "_r =" <+> prettyNatural r
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
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator of projective" <+> curve')
  <$$> "gP :: PP"
  <$$> "gP = P _x _y 1"
  <$$> prettyInline "gP"
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
