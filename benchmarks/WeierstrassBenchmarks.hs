module WeierstrassBenchmarks where

import Criterion.Main
import qualified Curve.Weierstrass.Anomalous       as Anomalous
import qualified Curve.Weierstrass.ANSSIFRP256V1   as ANSSIFRP256V1
import qualified Curve.Weierstrass.BLS12381        as BLS12381
import qualified Curve.Weierstrass.BLS12381T       as BLS12381T
import qualified Curve.Weierstrass.BLS48581        as BLS48581
import qualified Curve.Weierstrass.BLS48581T       as BLS48581T
import qualified Curve.Weierstrass.BN224           as BN224
import qualified Curve.Weierstrass.BN254           as BN254
import qualified Curve.Weierstrass.BN254T          as BN254T
import qualified Curve.Weierstrass.BN254A          as BN254A
import qualified Curve.Weierstrass.BN254AT         as BN254AT
import qualified Curve.Weierstrass.BN254B          as BN254B
import qualified Curve.Weierstrass.BN254BT         as BN254BT
import qualified Curve.Weierstrass.BN256           as BN256
import qualified Curve.Weierstrass.BN384           as BN384
import qualified Curve.Weierstrass.BN462           as BN462
import qualified Curve.Weierstrass.BN462T          as BN462T
import qualified Curve.Weierstrass.BN512           as BN512
import qualified Curve.Weierstrass.BrainpoolP160R1 as BrainpoolP160R1
import qualified Curve.Weierstrass.BrainpoolP160T1 as BrainpoolP160T1
import qualified Curve.Weierstrass.BrainpoolP192R1 as BrainpoolP192R1
import qualified Curve.Weierstrass.BrainpoolP192T1 as BrainpoolP192T1
import qualified Curve.Weierstrass.BrainpoolP224R1 as BrainpoolP224R1
import qualified Curve.Weierstrass.BrainpoolP224T1 as BrainpoolP224T1
import qualified Curve.Weierstrass.BrainpoolP256R1 as BrainpoolP256R1
import qualified Curve.Weierstrass.BrainpoolP256T1 as BrainpoolP256T1
import qualified Curve.Weierstrass.BrainpoolP320R1 as BrainpoolP320R1
import qualified Curve.Weierstrass.BrainpoolP320T1 as BrainpoolP320T1
import qualified Curve.Weierstrass.BrainpoolP384R1 as BrainpoolP384R1
import qualified Curve.Weierstrass.BrainpoolP384T1 as BrainpoolP384T1
import qualified Curve.Weierstrass.BrainpoolP512R1 as BrainpoolP512R1
import qualified Curve.Weierstrass.BrainpoolP512T1 as BrainpoolP512T1
import qualified Curve.Weierstrass.SECP112R1       as SECP112R1
import qualified Curve.Weierstrass.SECP112R2       as SECP112R2
import qualified Curve.Weierstrass.SECP128R1       as SECP128R1
import qualified Curve.Weierstrass.SECP128R2       as SECP128R2
import qualified Curve.Weierstrass.SECP160K1       as SECP160K1
import qualified Curve.Weierstrass.SECP160R1       as SECP160R1
import qualified Curve.Weierstrass.SECP160R2       as SECP160R2
import qualified Curve.Weierstrass.SECP192K1       as SECP192K1
import qualified Curve.Weierstrass.SECP192R1       as SECP192R1
import qualified Curve.Weierstrass.SECP224K1       as SECP224K1
import qualified Curve.Weierstrass.SECP224R1       as SECP224R1
import qualified Curve.Weierstrass.SECP256K1       as SECP256K1
import qualified Curve.Weierstrass.SECP256R1       as SECP256R1
import qualified Curve.Weierstrass.SECP384R1       as SECP384R1
import qualified Curve.Weierstrass.SECP521R1       as SECP521R1

import GroupBenchmarks

benchmarkWeierstrass :: Benchmark
benchmarkWeierstrass = bgroup "Weierstrass"
  [ benchmarkWeierstrass'       "Anomalous"       Anomalous.gA       Anomalous.gJ       Anomalous.gP
  , benchmarkWeierstrass'   "ANSSIFRP256V1"   ANSSIFRP256V1.gA   ANSSIFRP256V1.gJ   ANSSIFRP256V1.gP
  , benchmarkWeierstrass'        "BLS12381"        BLS12381.gA        BLS12381.gJ        BLS12381.gP
  , benchmarkWeierstrass'       "BLS12381T"       BLS12381T.gA       BLS12381T.gJ       BLS12381T.gP
  , benchmarkWeierstrass'        "BLS48581"        BLS48581.gA        BLS48581.gJ        BLS48581.gP
  , benchmarkWeierstrass'       "BLS48581T"       BLS48581T.gA       BLS48581T.gJ       BLS48581T.gP
  , benchmarkWeierstrass'           "BN224"           BN224.gA           BN224.gJ           BN224.gP
  , benchmarkWeierstrass'           "BN254"           BN254.gA           BN254.gJ           BN254.gP
  , benchmarkWeierstrass'          "BN254T"          BN254T.gA          BN254T.gJ          BN254T.gP
  , benchmarkWeierstrass'          "BN254A"          BN254A.gA          BN254A.gJ          BN254A.gP
  , benchmarkWeierstrass'         "BN254AT"         BN254AT.gA         BN254AT.gJ         BN254AT.gP
  , benchmarkWeierstrass'          "BN254B"          BN254B.gA          BN254B.gJ          BN254B.gP
  , benchmarkWeierstrass'         "BN254BT"         BN254BT.gA         BN254BT.gJ         BN254BT.gP
  , benchmarkWeierstrass'           "BN256"           BN256.gA           BN256.gJ           BN256.gP
  , benchmarkWeierstrass'           "BN384"           BN384.gA           BN384.gJ           BN384.gP
  , benchmarkWeierstrass'           "BN462"           BN462.gA           BN462.gJ           BN462.gP
  , benchmarkWeierstrass'          "BN462T"          BN462T.gA          BN462T.gJ          BN462T.gP
  , benchmarkWeierstrass'           "BN512"           BN512.gA           BN512.gJ           BN512.gP
  , benchmarkWeierstrass' "BrainpoolP160R1" BrainpoolP160R1.gA BrainpoolP160R1.gJ BrainpoolP160R1.gP
  , benchmarkWeierstrass' "BrainpoolP160T1" BrainpoolP160T1.gA BrainpoolP160T1.gJ BrainpoolP160T1.gP
  , benchmarkWeierstrass' "BrainpoolP192R1" BrainpoolP192R1.gA BrainpoolP192R1.gJ BrainpoolP192R1.gP
  , benchmarkWeierstrass' "BrainpoolP192T1" BrainpoolP192T1.gA BrainpoolP192T1.gJ BrainpoolP192T1.gP
  , benchmarkWeierstrass' "BrainpoolP224R1" BrainpoolP224R1.gA BrainpoolP224R1.gJ BrainpoolP224R1.gP
  , benchmarkWeierstrass' "BrainpoolP224T1" BrainpoolP224T1.gA BrainpoolP224T1.gJ BrainpoolP224T1.gP
  , benchmarkWeierstrass' "BrainpoolP256R1" BrainpoolP256R1.gA BrainpoolP256R1.gJ BrainpoolP256R1.gP
  , benchmarkWeierstrass' "BrainpoolP256T1" BrainpoolP256T1.gA BrainpoolP256T1.gJ BrainpoolP256T1.gP
  , benchmarkWeierstrass' "BrainpoolP320R1" BrainpoolP320R1.gA BrainpoolP320R1.gJ BrainpoolP320R1.gP
  , benchmarkWeierstrass' "BrainpoolP320T1" BrainpoolP320T1.gA BrainpoolP320T1.gJ BrainpoolP320T1.gP
  , benchmarkWeierstrass' "BrainpoolP384R1" BrainpoolP384R1.gA BrainpoolP384R1.gJ BrainpoolP384R1.gP
  , benchmarkWeierstrass' "BrainpoolP384T1" BrainpoolP384T1.gA BrainpoolP384T1.gJ BrainpoolP384T1.gP
  , benchmarkWeierstrass' "BrainpoolP512R1" BrainpoolP512R1.gA BrainpoolP512R1.gJ BrainpoolP512R1.gP
  , benchmarkWeierstrass' "BrainpoolP512T1" BrainpoolP512T1.gA BrainpoolP512T1.gJ BrainpoolP512T1.gP
  , benchmarkWeierstrass'       "SECP112R1"       SECP112R1.gA       SECP112R1.gJ       SECP112R1.gP
  , benchmarkWeierstrass'       "SECP112R2"       SECP112R2.gA       SECP112R2.gJ       SECP112R2.gP
  , benchmarkWeierstrass'       "SECP128R1"       SECP128R1.gA       SECP128R1.gJ       SECP128R1.gP
  , benchmarkWeierstrass'       "SECP128R2"       SECP128R2.gA       SECP128R2.gJ       SECP128R2.gP
  , benchmarkWeierstrass'       "SECP160K1"       SECP160K1.gA       SECP160K1.gJ       SECP160K1.gP
  , benchmarkWeierstrass'       "SECP160R1"       SECP160R1.gA       SECP160R1.gJ       SECP160R1.gP
  , benchmarkWeierstrass'       "SECP160R2"       SECP160R2.gA       SECP160R2.gJ       SECP160R2.gP
  , benchmarkWeierstrass'       "SECP192K1"       SECP192K1.gA       SECP192K1.gJ       SECP192K1.gP
  , benchmarkWeierstrass'       "SECP192R1"       SECP192R1.gA       SECP192R1.gJ       SECP192R1.gP
  , benchmarkWeierstrass'       "SECP224K1"       SECP224K1.gA       SECP224K1.gJ       SECP224K1.gP
  , benchmarkWeierstrass'       "SECP224R1"       SECP224R1.gA       SECP224R1.gJ       SECP224R1.gP
  , benchmarkWeierstrass'       "SECP256K1"       SECP256K1.gA       SECP256K1.gJ       SECP256K1.gP
  , benchmarkWeierstrass'       "SECP256R1"       SECP256R1.gA       SECP256R1.gJ       SECP256R1.gP
  , benchmarkWeierstrass'       "SECP384R1"       SECP384R1.gA       SECP384R1.gJ       SECP384R1.gP
  , benchmarkWeierstrass'       "SECP521R1"       SECP521R1.gA       SECP521R1.gJ       SECP521R1.gP
  ]
  where
    benchmarkWeierstrass' curve affine jacobian projective = bgroup curve
      [ benchmark "Affine" affine
      , benchmark "Jacobian" jacobian
      , benchmark "Projective" projective
      ]
