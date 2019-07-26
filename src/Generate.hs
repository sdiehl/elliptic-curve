module Generate
  ( generateCurve
  , generateGenerate
  ) where

import Protolude

import qualified Generate.Binary.Curve as Binary
import qualified Generate.Binary.Generate as Binary
import qualified Generate.Binary.Parameters as Binary
import qualified Generate.Edwards.Curve as Edwards
import qualified Generate.Edwards.Generate as Edwards
import qualified Generate.Edwards.Parameters as Edwards
import qualified Generate.Montgomery.Curve as Montgomery
import qualified Generate.Montgomery.Generate as Montgomery
import qualified Generate.Montgomery.Parameters as Montgomery
import qualified Generate.Weierstrass.Curve as Weierstrass
import qualified Generate.Weierstrass.Generate as Weierstrass
import qualified Generate.Weierstrass.Parameters as Weierstrass

-------------------------------------------------------------------------------
-- Curve generator
-------------------------------------------------------------------------------

generateCurve :: IO ()
generateCurve = do
  mapM_ putStrLn generateBinaryCurve
  mapM_ putStrLn generateEdwardsCurve
  mapM_ putStrLn generateMontgomeryCurve
  mapM_ putStrLn generateWeierstrassCurve

generateBinaryCurve :: [Text]
generateBinaryCurve = map (show . Binary.prettyCurve) Binary.curves

generateEdwardsCurve :: [Text]
generateEdwardsCurve = map (show . Edwards.prettyCurve) Edwards.curves

generateMontgomeryCurve :: [Text]
generateMontgomeryCurve = map (show . Montgomery.prettyCurve) Montgomery.curves

generateWeierstrassCurve :: [Text]
generateWeierstrassCurve = map (show . Weierstrass.prettyCurve) Weierstrass.curves

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
generateBinaryGenerate = show (Binary.prettyGenerate Binary.curves)

generateEdwardsGenerate :: Text
generateEdwardsGenerate = show (Edwards.prettyGenerate Edwards.curves)

generateMontgomeryGenerate :: Text
generateMontgomeryGenerate = show (Montgomery.prettyGenerate Montgomery.curves)

generateWeierstrassGenerate :: Text
generateWeierstrassGenerate = show (Weierstrass.prettyGenerate Weierstrass.curves)
