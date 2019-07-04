module Main where

import Protolude

import Criterion.Main
import Curve
import qualified Curve.ShortWeierstrass.BLS12_381.G1 as BLS12_381.G1
import qualified Curve.ShortWeierstrass.BLS12_381.G2 as BLS12_381.G2
import qualified Curve.TwistedEdwards.JubJub as JubJub

jubjub_p1 :: JubJub.P
jubjub_p1 = JubJub.A 0 1

jubjub_p2 :: JubJub.P
jubjub_p2 = JubJub.A 0 (-1)

bls12_381_g1_p1 :: BLS12_381.G1.P
bls12_381_g1_p1 = BLS12_381.G1.A 0 1

bls12_381_g1_p2 :: BLS12_381.G1.P
bls12_381_g1_p2 = BLS12_381.G1.A 0 (-1)

bls12_381_g2_p1 :: BLS12_381.G2.P
bls12_381_g2_p1 = BLS12_381.G2.A 0 1

bls12_381_g2_p2 :: BLS12_381.G2.P
bls12_381_g2_p2 = BLS12_381.G2.A 0 (-1)

main :: IO ()
main = defaultMain
  [ bgroup "JubJub"
    [ bench "Addition"
      $ whnf (uncurry (<>)) (jubjub_p1, jubjub_p2)
    , bench "Doubling"
      $ whnf double jubjub_p1
    , bench "Inversion"
      $ whnf inv jubjub_p1
    , bench "Multiplication"
      $ whnf (mul 100) jubjub_p1
    ]
  , bgroup "BLS12-381"
    [ bgroup "G1"
      [ bench "Addition"
        $ whnf (uncurry (<>)) (bls12_381_g1_p1, bls12_381_g1_p2)
      , bench "Doubling"
        $ whnf double bls12_381_g1_p1
      , bench "Inversion"
        $ whnf inv bls12_381_g1_p1
      , bench "Multiplication"
        $ whnf (mul 100) bls12_381_g1_p1
      ]
    , bgroup "G2"
      [ bench "Addition"
        $ whnf (uncurry (<>)) (bls12_381_g2_p1, bls12_381_g2_p2)
      , bench "Doubling"
        $ whnf double bls12_381_g2_p1
      , bench "Inversion"
        $ whnf inv bls12_381_g2_p1
      , bench "Multiplication"
        $ whnf (mul 100) bls12_381_g2_p1
      ]
    ]
  ]
