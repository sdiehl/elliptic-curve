module Main where

import Protolude

import Criterion.Main

import EllipticCurve
import qualified EllipticCurve.JubJub as JubJub
import qualified EllipticCurve.BLS12_381 as BLS12_381

jubjub_p1 :: JubJub.Point
jubjub_p1 = JubJub.A 0 1

jubjub_p2 :: JubJub.Point
jubjub_p2 = JubJub.A 0 (-1)

bls12_381_g1_p1 :: BLS12_381.Point_G1
bls12_381_g1_p1 = BLS12_381.A_G1 0 1

bls12_381_g1_p2 :: BLS12_381.Point_G1
bls12_381_g1_p2 = BLS12_381.A_G1 0 (-1)

bls12_381_g2_p1 :: BLS12_381.Point_G2
bls12_381_g2_p1 = BLS12_381.A_G2 0 1

bls12_381_g2_p2 :: BLS12_381.Point_G2
bls12_381_g2_p2 = BLS12_381.A_G2 0 (-1)

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
      $ whnf (scale 100) jubjub_p1
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
        $ whnf (scale 100) bls12_381_g1_p1
      ]
    , bgroup "G2"
      [ bench "Addition"
        $ whnf (uncurry (<>)) (bls12_381_g2_p1, bls12_381_g2_p2)
      , bench "Doubling"
        $ whnf double bls12_381_g2_p1
      , bench "Inversion"
        $ whnf inv bls12_381_g2_p1
      , bench "Multiplication"
        $ whnf (scale 100) bls12_381_g2_p1
      ]
    ]
  ]
