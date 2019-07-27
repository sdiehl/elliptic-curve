module EdwardsTests where

import qualified Curve.Edwards.Curve1174  as Curve1174
import qualified Curve.Edwards.Curve41417 as Curve41417
import qualified Curve.Edwards.E222       as E222
import qualified Curve.Edwards.E382       as E382
import qualified Curve.Edwards.E521       as E521
import qualified Curve.Edwards.Ed448      as Ed448
import qualified Curve.Edwards.Ed3363     as Ed3363
import qualified Curve.Edwards.Ed25519    as Ed25519
import qualified Curve.Edwards.JubJub     as JubJub
import Test.Tasty

import CurveTests

testEdwards :: TestTree
testEdwards = testGroup "Edwards"
  [ test  "Curve1174"  Curve1174._g  Curve1174._h  Curve1174._n  Curve1174._p
  , test "Curve41417" Curve41417._g Curve41417._h Curve41417._n Curve41417._p
  , test       "E222"       E222._g       E222._h       E222._n       E222._p
  , test       "E382"       E382._g       E382._h       E382._n       E382._p
  , test       "E521"       E521._g       E521._h       E521._n       E521._p
  , test      "Ed448"      Ed448._g      Ed448._h      Ed448._n      Ed448._p
  , test     "Ed3363"     Ed3363._g     Ed3363._h     Ed3363._n     Ed3363._p
  , test    "Ed25519"    Ed25519._g    Ed25519._h    Ed25519._n    Ed25519._p
  , test     "JubJub"     JubJub._g     JubJub._h     JubJub._n     JubJub._p
  ]
