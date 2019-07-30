module Curve.Weierstrass.BLS48581T
  ( AP
  , Curve(..)
  , Fq8
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import ExtensionField
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Curve.Weierstrass.BLS48581 (Fq)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BLS48581T curve
-------------------------------------------------------------------------------

-- | BLS48581T curve.
data BLS48581T

-- | Field of points of BLS48581T curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x * x + 1
type Fq2 = ExtensionField Fq PolynomialU
data PolynomialV
instance IrreducibleMonic Fq2 PolynomialV where
  split _ = x * x + 1 + t x
type Fq4 = ExtensionField Fq2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fq4 PolynomialW where
  split _ = x * x + t x
type Fq8 = ExtensionField Fq4 PolynomialW

-- | Field of coefficients of BLS48581T curve.
type Fr = PrimeField 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01

-- | BLS48581T curve is a Weierstrass curve.
instance Curve 'Weierstrass c BLS48581T Fq8 => WCurve c BLS48581T Fq8 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of BLS48581T curve.
_a :: Fq8
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS48581T curve.
_b :: Fq8
_b = fromList [ fromList [
                         ]
              , fromList [ fromList [
                                    ]
                         , fromList [ 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c895
                                    , 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c896
                                    ]
                         ]
              ]
{-# INLINE _b #-}

-- | Cofactor of BLS48581T curve.
_h :: Integer
_h = 0x170e915cb0a6b7406b8d94042317f811d6bc3fc6e211ada42e58ccfcb3ac076a7e4499d700a0c23dc4b0c078f92def8c87b7fe63e1eea270db353a4ef4d38b5998ad8f0d042ea24c8f02be1c0c83992fe5d7725227bb27123a949e0876c0a8ce0a67326db0e955dcb791b867f31d6bfa62fbdd5f44a00504df04e186fae033f1eb43c1b1a08b6e086eff03c8fee9ebdd1e191a8a4b0466c90b389987de5637d5dd13dab33196bd2e5afa6cd19cf0fc3fc7db7ece1f3fac742626b1b02fcee04043b2ea96492f6afa51739597c54bb78aa6b0b99319fef9d09f768831018ee6564c68d054c62f2e0b4549426fec24ab26957a669dba2a2b6945ce40c9aec6afdeda16c79e15546cd7771fa544d5364236690ea06832679562a68731420ae52d0d35a90b8d10b688e31b6aee45f45b7a5083c71732105852decc888f64839a4de33b99521f0984a418d20fc7b0609530e454f0696fa2a8075ac01cc8ae3869e8d0fe1f3788ffac4c01aa2720e431da333c83d9663bfb1fb7a1a7b90528482c6be7892299030bb51a51dc7e91e9156874416bf4c26f1ea7ec578058563960ef92bbbb8632d3a1b695f954af10e9a78e40acffc13b06540aae9da5287fc4429485d44e6289d8c0d6a3eb2ece35012452751839fb48bc14b515478e2ff412d930ac20307561f3a5c998e6bcbfebd97effc6433033a2361bfcdc4fc74ad379a16c6dea49c209b1
{-# INLINE _h #-}

-- | Characteristic of BLS48581T curve.
_q :: Integer
_q = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
{-# INLINE _q #-}

-- | Order of BLS48581T curve.
_r :: Integer
_r = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BLS48581T point.
type AP = WAPoint BLS48581T Fq8

-- | Affine BLS48581T curve is a Weierstrass affine curve.
instance WACurve BLS48581T Fq8 where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BLS48581T curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BLS48581T curve.
xA :: Fq8
xA = fromList [ fromList [ fromList [ 0x5d615d9a7871e4a38237fa45a2775debabbefc70344dbccb7de64db3a2ef156c46ff79baad1a8c42281a63ca0612f400503004d80491f510317b79766322154dec34fd0b4ace8bfab
                                    , 0x7c4973ece2258512069b0e86abc07e8b22bb6d980e1623e9526f6da12307f4e1c3943a00abfedf16214a76affa62504f0c3c7630d979630ffd75556a01afa143f1669b36676b47c57
                                    ]
                         , fromList [ 0x1fccc70198f1334e1b2ea1853ad83bc73a8a6ca9ae237ca7a6d6957ccbab5ab6860161c1dbd19242ffae766f0d2a6d55f028cbdfbb879d5fea8ef4cded6b3f0b46488156ca55a3e6a
                                    , 0xbe2218c25ceb6185c78d8012954d4bfe8f5985ac62f3e5821b7b92a393f8be0cc218a95f63e1c776e6ec143b1b279b9468c31c5257c200ca52310b8cb4e80bc3f09a7033cbb7feafe
                                    ]
                         ]
              , fromList [ fromList [ 0x38b91c600b35913a3c598e4caa9dd63007c675d0b1642b5675ff0e7c5805386699981f9e48199d5ac10b2ef492ae589274fad55fc1889aa80c65b5f746c9d4cbb739c3a1c53f8cce5
                                    , 0xc96c7797eb0738603f1311e4ecda088f7b8f35dcef0977a3d1a58677bb037418181df63835d28997eb57b40b9c0b15dd7595a9f177612f097fc7960910fce3370f2004d914a3c093a
                                    ]
                         , fromList [ 0xb9b7951c6061ee3f0197a498908aee660dea41b39d13852b6db908ba2c0b7a449cef11f293b13ced0fd0caa5efcf3432aad1cbe4324c22d63334b5b0e205c3354e41607e60750e057
                                    , 0x827d5c22fb2bdec5282624c4f4aaa2b1e5d7a9defaf47b5211cf741719728a7f9f8cfca93f29cff364a7190b7e2b0d4585479bd6aebf9fc44e56af2fc9e97c3f84e19da00fbc6ae34
                                    ]
                         ]
              ]
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BLS48581T curve.
yA :: Fq8
yA = fromList [ fromList [ fromList [ 0xeb53356c375b5dfa497216452f3024b918b4238059a577e6f3b39ebfc435faab0906235afa27748d90f7336d8ae5163c1599abf77eea6d659045012ab12c0ff323edd3fe4d2d7971
                                    , 0x284dc75979e0ff144da6531815fcadc2b75a422ba325e6fba01d72964732fcbf3afb096b243b1f192c5c3d1892ab24e1dd212fa097d760e2e588b423525ffc7b111471db936cd5665
                                    ]
                         , fromList [ 0xb36a201dd008523e421efb70367669ef2c2fc5030216d5b119d3a480d370514475f7d5c99d0e90411515536ca3295e5e2f0c1d35d51a652269cbc7c46fc3b8fde68332a526a2a8474
                                    , 0xaec25a4621edc0688223fbbd478762b1c2cded3360dcee23dd8b0e710e122d2742c89b224333fa40dced2817742770ba10d67bda503ee5e578fb3d8b8a1e5337316213da92841589d
                                    ]
                         ]
              , fromList [ fromList [ 0xd209d5a223a9c46916503fa5a88325a2554dc541b43dd93b5a959805f1129857ed85c77fa238cdce8a1e2ca4e512b64f59f430135945d137b08857fdddfcf7a43f47831f982e50137
                                    , 0x7d0d03745736b7a513d339d5ad537b90421ad66eb16722b589d82e2055ab7504fa83420e8c270841f6824f47c180d139e3aafc198caa72b679da59ed8226cf3a594eedc58cf90bee4
                                    ]
                         , fromList [ 0x896767811be65ea25c2d05dfdd17af8a006f364fc0841b064155f14e4c819a6df98f425ae3a2864f22c1fab8c74b2618b5bb40fa639f53dccc9e884017d9aa62b3d41faeafeb23986
                                    , 0x35e2524ff89029d393a5c07e84f981b5e068f1406be8e50c87549b6ef8eca9a9533a3f8e69c31e97e1ad0333ec719205417300d8c4ab33f748e5ac66e84069c55d667ffcb732718b6
                                    ]
                         ]
              ]
{-# INLINE yA #-}
