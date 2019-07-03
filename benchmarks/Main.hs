module Main where

import Protolude

import Criterion.Main
import qualified Curve as C
import qualified Curve.TwistedEdwards.JubJub as JubJub
import qualified Curve.ShortWeierstrass.BLS12_381.G1 as BLS12_381.G1
import qualified Curve.ShortWeierstrass.BLS12_381.G2 as BLS12_381.G2

jubjub_p1 :: JubJub.Point
jubjub_p1 = JubJub.A 0 1

jubjub_p2 :: JubJub.Point
jubjub_p2 = JubJub.A 0 (-1)

bls12_381_g1_p1 :: BLS12_381.G1.Point
bls12_381_g1_p1 = BLS12_381.G1.A 0 1

bls12_381_g1_p2 :: BLS12_381.G1.Point
bls12_381_g1_p2 = BLS12_381.G1.A 0 (-1)

bls12_381_g2_p1 :: BLS12_381.G2.Point
bls12_381_g2_p1 = BLS12_381.G2.A 0 1

bls12_381_g2_p2 :: BLS12_381.G2.Point
bls12_381_g2_p2 = BLS12_381.G2.A 0 (-1)

main :: IO ()
main = defaultMain
  [ bgroup "JubJub"
    [ bench "Addition"
      $ whnf (uncurry (<>)) (jubjub_p1, jubjub_p2)
    , bench "Doubling"
      $ whnf C.double jubjub_p1
    , bench "Inversion"
      $ whnf C.inv jubjub_p1
    , bench "Multiplication"
      $ whnf (C.scale 100) jubjub_p1
    ]
  , bgroup "BLS12-381"
    [ bgroup "G1"
      [ bench "Addition"
        $ whnf (uncurry (<>)) (bls12_381_g1_p1, bls12_381_g1_p2)
      , bench "Doubling"
        $ whnf C.double bls12_381_g1_p1
      , bench "Inversion"
        $ whnf C.inv bls12_381_g1_p1
      , bench "Multiplication"
        $ whnf (C.scale 100) bls12_381_g1_p1
      ]
    , bgroup "G2"
      [ bench "Addition"
        $ whnf (uncurry (<>)) (bls12_381_g2_p1, bls12_381_g2_p2)
      , bench "Doubling"
        $ whnf C.double bls12_381_g2_p1
      , bench "Inversion"
        $ whnf C.inv bls12_381_g2_p1
      , bench "Multiplication"
        $ whnf (C.scale 100) bls12_381_g2_p1
      ]
    ]
  ]
