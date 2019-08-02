module Generate.Binary.Curve
  ( prettyCurve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Binary.Types
import Generate.Curve
import Generate.Pretty

-------------------------------------------------------------------------------
-- Curve
-------------------------------------------------------------------------------

prettyImports :: Types -> Doc
prettyImports Types{..}
  =    "module Curve.Binary." <> pretty curve
  <$$> indent 2
    (    "( BCurve(..)"
    <$$> ", BPoint"
    <$$> ", BACurve(..)"
    <$$> ", BAPoint"
    <$$> ", BPCurve(..)"
    <$$> ", BPPoint"
    <$$> ", Curve(..)"
    <$$> "," <+> prettyField field
    <$$> ", Fr"
    <$$> ", Group(..)"
    <$$> ", PA"
    <$$> ", PP"
    <$$> ", _a"
    <$$> ", _b"
    <$$> ", _h"
    <$$> ", _p"
    <$$> ", _r"
    <$$> ", _x"
    <$$> ", _y"
    <$$> ", gA"
    <$$> ", gP"
    <$$> ", pattern A"
    <$$> ", pattern P"
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport field
  <>   prettyBreak
  <$$> "import Curve.Binary"
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
  <$$> prettyDocumentation (curve' <+> "is a binary curve")
  <$$> "instance Curve 'Binary c" <+> pretty curve <+> prettyField field
  <+>  "=> BCurve c" <+> pretty curve <+> prettyField field <+> "where"
  <$$> indent 2
    (    "a_ = const _a"
    <$$> prettyInline "a_"
    <$$> "b_ = const _b"
    <$$> prettyInline "b_"
    <$$> "h_ = const _h"
    <$$> prettyInline "h_"
    <$$> "p_ = const _p"
    <$$> prettyInline "p_"
    <$$> "r_ = const _r"
    <$$> prettyInline "r_"
    <$$> "x_ = const _x"
    <$$> prettyInline "x_"
    <$$> "y_ = const _y"
    <$$> prettyInline "y_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "point")
  <$$> "type PA = BAPoint" <+> pretty curve <+> prettyField field
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "is a binary affine curve")
  <$$> "instance BACurve" <+> pretty curve <+> prettyField field <+> "where"
  <$$> indent 2
    (    "gA_ = gA"
    <$$> prettyInline "gA_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Projective" <+> pretty curve <+> "point")
  <$$> "type PP = BPPoint" <+> pretty curve <+> prettyField field
  <>   prettyBreak
  <$$> prettyDocumentation ("Projective" <+> curve' <+> "is a binary projective curve")
  <$$> "instance BPCurve" <+> pretty curve <+> prettyField field <+> "where"
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
  <$$> prettyDocumentation ("Polynomial of" <+> curve')
  <$$> "_p :: Integer"
  <$$> "_p =" <+> prettyInteger p
  <$$> prettyInline "_p"
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
  <$$> "gA = fromMaybe (panic" <+> dquotes "not well-defined." <> ") (point _x _y)"
  <$$> prettyInline "gA"
  <>   prettyBreak
  <$$> prettyDocumentation ("Generator of projective" <+> curve')
  <$$> "gP :: PP"
  <$$> "gP = fromMaybe (panic" <+> dquotes "not well-defined." <> ") (point _x _y)"
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
