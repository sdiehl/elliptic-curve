module Curve.Field.BN254TF
  ( Fp6
  , Fp12
  , P
  , _g
  , _n
  , _p
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, t, x)

import Curve.Field (FGroup(..), Element(..))
import Curve.Weierstrass.BN254T (Fp2)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of BN254TF group
data PolynomialV
instance IrreducibleMonic Fp2 PolynomialV where
  split _ = x ^ (3 :: Int) - (9 + t x)
type Fp6 = ExtensionField Fp2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fp6 PolynomialW where
  split _ = x ^ (2 :: Int) - t x
type Fp12 = ExtensionField Fp6 PolynomialW

-- | BN254TF group is a field group
instance FGroup Fp12 where
  g_ = _g
  {-# INLINE g_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Element of BN254TF group
type P = Element Fp12

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Generator of BN254TF group
_g :: P
_g = F
  ( fromList
    [ fromList
      [ fromList
        [ 8493334370784016972005089913588211327688223499729897951716206968320726508021
        , 3758435817766288188804561253838670030762970764366672594784247447067868088068
        ]
      , fromList
        [ 6565798094314091391201231504228224566495939541538094766881371862976727043038
        , 14656606573936501743457633041048024656612227301473084805627390748872617280984
        ]
      , fromList
        [ 634997487638609332803583491743335852620873788902390365055086820718589720118
        , 19455424343576886430889849773367397946457449073528455097210946839000147698372
        ]
      ]
    , fromList
      [ fromList
        [ 20049218015652006197026173611347504489508678646783216776320737476707192559881
        , 18059168546148152671857026372711724379319778306792011146784665080987064164612
        ]
      , fromList
        [ 12145052038566888241256672223106590273978429515702193755778990643425246950730
        , 17918828665069491344039743589118342552553375221610735811112289083834142789347
        ]
      , fromList
        [ 6223602427219597392892794664899549544171383137467762280768257680446283161705
        , 7484542354754424633621663080190936924481536615300815203692506276894207018007
        ]
      ]
    ]
  )
{-# INLINE _g #-}

-- | Order of BN254TF group
_n :: Integer
_n = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _n #-}

-- | Characteristic of BN254TF group
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
