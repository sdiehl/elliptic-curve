module Main where

import Protolude

import Data.Text (unpack)

import qualified Generate.Binary.Curve as Binary
import qualified Generate.Binary.Generate as Binary
import qualified Generate.Binary.Parameters as Binary
import qualified Generate.Binary.Types as Binary
import qualified Generate.Edwards.Curve as Edwards
import qualified Generate.Edwards.Generate as Edwards
import qualified Generate.Edwards.Parameters as Edwards
import qualified Generate.Edwards.Types as Edwards
import qualified Generate.Montgomery.Curve as Montgomery
import qualified Generate.Montgomery.Generate as Montgomery
import qualified Generate.Montgomery.Parameters as Montgomery
import qualified Generate.Montgomery.Types as Montgomery
import qualified Generate.Weierstrass.Curve as Weierstrass
import qualified Generate.Weierstrass.Generate as Weierstrass
import qualified Generate.Weierstrass.Parameters as Weierstrass
import qualified Generate.Weierstrass.Types as Weierstrass

-------------------------------------------------------------------------------
-- Curve generator
-------------------------------------------------------------------------------

generateCurve :: IO ()
generateCurve = do
  mapM_ (uncurry writeFile) generateBinaryCurve
  mapM_ (uncurry writeFile) generateEdwardsCurve
  mapM_ (uncurry writeFile) generateMontgomeryCurve
  mapM_ (uncurry writeFile) generateWeierstrassCurve

generateBinaryCurve :: [(FilePath, Text)]
generateBinaryCurve = map generateBinaryCurve' Binary.curves
  where
    generateBinaryCurve' :: Binary.Curve -> (FilePath, Text)
    generateBinaryCurve' = generateCurve'
      "Binary" Binary.curve Binary.types Binary.prettyCurve

generateEdwardsCurve :: [(FilePath, Text)]
generateEdwardsCurve = map generateEdwardsCurve' Edwards.curves
  where
    generateEdwardsCurve' :: Edwards.Curve -> (FilePath, Text)
    generateEdwardsCurve' = generateCurve'
      "Edwards" Edwards.curve Edwards.types Edwards.prettyCurve

generateMontgomeryCurve :: [(FilePath, Text)]
generateMontgomeryCurve = map generateMontgomeryCurve' Montgomery.curves
  where
    generateMontgomeryCurve' :: Montgomery.Curve -> (FilePath, Text)
    generateMontgomeryCurve' = generateCurve'
      "Montgomery" Montgomery.curve Montgomery.types Montgomery.prettyCurve

generateWeierstrassCurve :: [(FilePath, Text)]
generateWeierstrassCurve = map generateWeierstrassCurve' Weierstrass.curves
  where
    generateWeierstrassCurve' :: Weierstrass.Curve -> (FilePath, Text)
    generateWeierstrassCurve' = generateCurve'
      "Weierstrass" Weierstrass.curve Weierstrass.types Weierstrass.prettyCurve

generateCurve' :: Show doc => Text -> (types -> Text)
  -> (curve -> types) -> (curve -> doc) -> curve -> (FilePath, Text)
generateCurve' form curve types pretty = (,)
  <$> unpack . (<> ".hs") . (<>) ("../src/Data/Curve/" <> form <> "/") . curve . types
  <*> show . pretty

-------------------------------------------------------------------------------
-- Generate generator
-------------------------------------------------------------------------------

generateGenerate :: IO ()
generateGenerate = do
  writeFile "Generate/Binary/Parameters.hs" generateBinaryGenerate
  writeFile "Generate/Edwards/Parameters.hs" generateEdwardsGenerate
  writeFile "Generate/Montgomery/Parameters.hs" generateMontgomeryGenerate
  writeFile "Generate/Weierstrass/Parameters.hs" generateWeierstrassGenerate

generateBinaryGenerate :: Text
generateBinaryGenerate = generateGenerate'
  Binary.prettyGenerate Binary.curves

generateEdwardsGenerate :: Text
generateEdwardsGenerate = generateGenerate'
  Edwards.prettyGenerate Edwards.curves

generateMontgomeryGenerate :: Text
generateMontgomeryGenerate = generateGenerate'
  Montgomery.prettyGenerate Montgomery.curves

generateWeierstrassGenerate :: Text
generateWeierstrassGenerate = generateGenerate'
  Weierstrass.prettyGenerate Weierstrass.curves

generateGenerate' :: Show doc => (curve -> doc) -> curve -> Text
generateGenerate' = (.) show

-------------------------------------------------------------------------------
-- Main generator
-------------------------------------------------------------------------------

main :: IO ()
main = generateGenerate >> generateCurve
