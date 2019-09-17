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

prettyImports :: Text -> Types -> Doc
prettyImports name Types{..}
  =    "module Data.Curve.Binary." <> pretty name
  <$$> indent 2
    (    "( module Data.Curve.Binary"
    <$$> "-- *" <+> pretty curve <+> "curve"
    <$$> ", module Data.Curve.Binary." <> pretty name
    <$$> ") where"
    )
  <>   prettyBreak
  <$$> prettyImport
  <>   prettyBreak
  <$$> "import Data.Curve.Binary"
  <$$> if curve == name then mempty else "import Data.Curve.Binary."
  <>   pretty curve <+> "(" <> pretty curve <> ", F2m, Fr)"

prettyTypes :: Types -> Parameters -> Doc
prettyTypes Types{..} Parameters{..}
  =    prettySection "Types"
  <$$> prettyType curve field p r
  <>   prettyBreak
  <$$> prettyComment (curve' <+> "is a binary curve")
  <$$> "instance Curve 'Binary c" <+> pretty curve <+> prettyField field
  <+>  "Fr => BCurve c" <+> pretty curve <+> prettyField field <+> "Fr where"
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
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Affine" <+> curve' <+> "point")
  <$$> "type PA = BAPoint" <+> pretty curve <+> prettyField field <+> "Fr"
  <>   prettyBreak
  <$$> prettyComment ("Affine" <+> curve' <+> "is a binary affine curve")
  <$$> "instance BACurve" <+> pretty curve <+> prettyField field <+> "Fr where"
  <$$> indent 2
    (    "gA_ = gA"
    <$$> prettyInline "gA_"
    )
  <>   prettyBreak
  <$$> prettyDocumentation ("Projective" <+> pretty curve <+> "point")
  <$$> "type PP = BPPoint" <+> pretty curve <+> prettyField field <+> "Fr"
  <>   prettyBreak
  <$$> prettyComment ("Projective" <+> curve' <+> "is a binary projective curve")
  <$$> "instance BPCurve" <+> pretty curve <+> prettyField field <+> "Fr where"
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
  <$$> "_h :: Natural"
  <$$> "_h =" <+> prettyNatural h
  <$$> prettyInline "_h"
  <>   prettyBreak
  <$$> prettyDocumentation ("Polynomial of" <+> curve')
  <$$> "_p :: Natural"
  <$$> "_p =" <+> prettyNatural p
  <$$> prettyInline "_p"
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
prettyCurve (Curve name types parameters)
  =    prettyImports name types
  <>   prettyBreak
  <$$> prettyTypes types parameters
  <>   prettyBreak
  <$$> prettyParameters types parameters
  <>   prettyBreak
