module MontgomeryTests where

import qualified Curve.Montgomery.Curve448    as Curve448
import qualified Curve.Montgomery.Curve25519  as Curve25519
import qualified Curve.Montgomery.Curve383187 as Curve383187
import qualified Curve.Montgomery.M221        as M221
import qualified Curve.Montgomery.M383        as M383
import qualified Curve.Montgomery.M511        as M511
import Test.Tasty

import GroupTests

testMontgomery :: TestTree
testMontgomery = testGroup "Montgomery"
  [ testGroup "Affine"
    [ test    "Curve448"    Curve448.gA    Curve448._h    Curve448._q    Curve448._r
    , test  "Curve25519"  Curve25519.gA  Curve25519._h  Curve25519._q  Curve25519._r
    , test "Curve383187" Curve383187.gA Curve383187._h Curve383187._q Curve383187._r
    , test        "M221"        M221.gA        M221._h        M221._q        M221._r
    , test        "M383"        M383.gA        M383._h        M383._q        M383._r
    , test        "M511"        M511.gA        M511._h        M511._q        M511._r
    ]
  ]
