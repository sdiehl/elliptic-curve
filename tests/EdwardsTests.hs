module EdwardsTests where

import Protolude

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
import Test.Tasty.QuickCheck

import GroupTests

testEdwards :: TestTree
testEdwards = testGroup "Edwards"
  [ testEdwards' 100  "Curve1174"  Curve1174._h  Curve1174._q  Curve1174._r  Curve1174.gA  Curve1174.gP
  , testEdwards' 100 "Curve41417" Curve41417._h Curve41417._q Curve41417._r Curve41417.gA Curve41417.gP
  , testEdwards' 100       "E222"       E222._h       E222._q       E222._r       E222.gA       E222.gP
  , testEdwards' 100       "E382"       E382._h       E382._q       E382._r       E382.gA       E382.gP
  , testEdwards' 100       "E521"       E521._h       E521._q       E521._r       E521.gA       E521.gP
  , testEdwards' 100      "Ed448"      Ed448._h      Ed448._q      Ed448._r      Ed448.gA      Ed448.gP
  , testEdwards' 100     "Ed3363"     Ed3363._h     Ed3363._q     Ed3363._r     Ed3363.gA     Ed3363.gP
  , testEdwards' 100    "Ed25519"    Ed25519._h    Ed25519._q    Ed25519._r    Ed25519.gA    Ed25519.gP
  , testEdwards' 100     "JubJub"     JubJub._h     JubJub._q     JubJub._r     JubJub.gA     JubJub.gP
  ]
  where
    testEdwards' n c h q r a p = localOption (QuickCheckTests n) $ testGroup c
      [ test "Affine" a h q r
      , test "Projective" p h q r
      ]
