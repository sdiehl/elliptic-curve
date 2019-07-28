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
  [ test    "Curve448"    Curve448._g    Curve448._h    Curve448._n    Curve448._p
  , test  "Curve25519"  Curve25519._g  Curve25519._h  Curve25519._n  Curve25519._p
  , test "Curve383187" Curve383187._g Curve383187._h Curve383187._n Curve383187._p
  , test        "M221"        M221._g        M221._h        M221._n        M221._p
  , test        "M383"        M383._g        M383._h        M383._n        M383._p
  , test        "M511"        M511._g        M511._h        M511._n        M511._p
  ]
