module Generate
  ( generateGenerate
  ) where

import Protolude

import Generate.Curve.Binary (binaryCurves)
import Generate.Curve.Edwards (edwardsCurves)
import Generate.Curve.Montgomery (montgomeryCurves)
import Generate.Curve.Weierstrass (weierstrassCurves)
import Generate.Generate (prettyGenerate)

-------------------------------------------------------------------------------
-- Generate generator
-------------------------------------------------------------------------------

generateGenerate :: IO ()
generateGenerate = do
  writeFile "Generate/Curve/Binary.hs" . show $
    prettyGenerate "Binary" binaryCurves
  writeFile "Generate/Curve/Edwards.hs" . show $
    prettyGenerate "Edwards" edwardsCurves
  writeFile "Generate/Curve/Montgomery.hs" . show $
    prettyGenerate "Montgomery" montgomeryCurves
  writeFile "Generate/Curve/Weierstrass.hs" . show $
    prettyGenerate "Weierstrass" weierstrassCurves
