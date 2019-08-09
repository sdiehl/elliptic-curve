module Group.Field.BN254TF
  ( module Group
  , module Group.Field
  , module Group.Field.BN254TF
  ) where

import Protolude

import ExtensionField

import Curve.Weierstrass.BN254T (Fq2)
import Group
import Group.Field

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of BN254TF group.
data PolynomialV
instance IrreducibleMonic Fq2 PolynomialV where
  split _ = X3 - Y X - 9
type Fq6 = ExtensionField Fq2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fq6 PolynomialW where
  split _ = X2 - Y X
type Fq12 = ExtensionField Fq6 PolynomialW

-- | BN254TF group is a field group.
instance FGroup Fq12 where
  g_ = _g
  {-# INLINABLE g_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = _x
  {-# INLINABLE x_ #-}

-- | Element of BN254TF group.
type P = Element Fq12

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Generator of BN254TF group.
_g :: P
_g = F _x
{-# INLINABLE _g #-}

-- | Characteristic of BN254TF group.
_q :: Integer
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINABLE _q #-}

-- | Order of BN254TF group.
_r :: Integer
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINABLE _r #-}

-- | Element @X@ of BN254TF group.
_x :: Fq12
_x = toField [ toField [ toField [ 0x12c70e90e12b7874510cd1707e8856f71bf7f61d72631e268fca81000db9a1f5
                                 , 0x84f330485b09e866bc2f2ea2b897394deaf3f12aa31f28cb0552990967d4704
                                 ]
                       , toField [ 0xe841c2ac18a4003ac9326b9558380e0bc27fdd375e3605f96b819a358d34bde
                                 , 0x2067586885c3318eeffa1938c754fe3c60224ee5ae15e66af6b5104c47c8c5d8
                                 ]
                       , toField [ 0x1676555de427abc409c4a394bc5426886302996919d4bf4bdd02236e14b3636
                                 , 0x2b03614464f04dd772d86df88674c270ffc8747ea13e72da95e3594468f222c4
                                 ]
                       ]
             , toField [ toField [ 0x2c53748bcd21a7c038fb30ddc8ac3bf0af25d7859cfbc12c30c866276c565909
                                 , 0x27ed208e7a0b55ae6e710bbfbd2fd922669c026360e37cc5b2ab862411536104
                                 ]
                       , toField [ 0x1ad9db1937fd72f4ac462173d31d3d6117411fa48dba8d499d762b47edb3b54a
                                 , 0x279db296f9d479292532c7c493d8e0722b6efae42158387564889c79fc038ee3
                                 ]
                       , toField [ 0xdc26f240656bbe2029bd441d77c221f0ba4c70c94b29b5f17f0f6d08745a069
                                 , 0x108c19d15f9446f744d0f110405d3856d6cc3bda6c4d537663729f5257628417
                                 ]
                       ]
             ]
{-# INLINABLE _x #-}
