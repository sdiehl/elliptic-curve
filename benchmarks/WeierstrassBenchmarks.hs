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
  [ bgroup       "Anomalous"
    [ benchmark "Affine"       Anomalous.gA
    ]
  , bgroup   "ANSSIFRP256V1"
    [ benchmark "Affine"   ANSSIFRP256V1.gA
    ]
  , bgroup        "BLS12381"
    [ benchmark "Affine"        BLS12381.gA
    ]
  , bgroup       "BLS12381T"
    [ benchmark "Affine"       BLS12381T.gA
    ]
  , bgroup        "BLS48581"
    [ benchmark "Affine"        BLS48581.gA
    ]
  , bgroup       "BLS48581T"
    [ benchmark "Affine"       BLS48581T.gA
    ]
  , bgroup           "BN224"
    [ benchmark "Affine"           BN224.gA
    ]
  , bgroup           "BN254"
    [ benchmark "Affine"           BN254.gA
    ]
  , bgroup          "BN254T"
    [ benchmark "Affine"          BN254T.gA
    ]
  , bgroup          "BN254A"
    [ benchmark "Affine"          BN254A.gA
    ]
  , bgroup         "BN254AT"
    [ benchmark "Affine"         BN254AT.gA
    ]
  , bgroup          "BN254B"
    [ benchmark "Affine"          BN254B.gA
    ]
  , bgroup         "BN254BT"
    [ benchmark "Affine"         BN254BT.gA
    ]
  , bgroup           "BN256"
    [ benchmark "Affine"           BN256.gA
    ]
  , bgroup           "BN384"
    [ benchmark "Affine"           BN384.gA
    ]
  , bgroup           "BN462"
    [ benchmark "Affine"           BN462.gA
    ]
  , bgroup          "BN462T"
    [ benchmark "Affine"          BN462T.gA
    ]
  , bgroup           "BN512"
    [ benchmark "Affine"           BN512.gA
    ]
  , bgroup "BrainpoolP160R1"
    [ benchmark "Affine" BrainpoolP160R1.gA
    ]
  , bgroup "BrainpoolP160T1"
    [ benchmark "Affine" BrainpoolP160T1.gA
    ]
  , bgroup "BrainpoolP192R1"
    [ benchmark "Affine" BrainpoolP192R1.gA
    ]
  , bgroup "BrainpoolP192T1"
    [ benchmark "Affine" BrainpoolP192T1.gA
    ]
  , bgroup "BrainpoolP224R1"
    [ benchmark "Affine" BrainpoolP224R1.gA
    ]
  , bgroup "BrainpoolP224T1"
    [ benchmark "Affine" BrainpoolP224T1.gA
    ]
  , bgroup "BrainpoolP256R1"
    [ benchmark "Affine" BrainpoolP256R1.gA
    ]
  , bgroup "BrainpoolP256T1"
    [ benchmark "Affine" BrainpoolP256T1.gA
    ]
  , bgroup "BrainpoolP320R1"
    [ benchmark "Affine" BrainpoolP320R1.gA
    ]
  , bgroup "BrainpoolP320T1"
    [ benchmark "Affine" BrainpoolP320T1.gA
    ]
  , bgroup "BrainpoolP384R1"
    [ benchmark "Affine" BrainpoolP384R1.gA
    ]
  , bgroup "BrainpoolP384T1"
    [ benchmark "Affine" BrainpoolP384T1.gA
    ]
  , bgroup "BrainpoolP512R1"
    [ benchmark "Affine" BrainpoolP512R1.gA
    ]
  , bgroup "BrainpoolP512T1"
    [ benchmark "Affine" BrainpoolP512T1.gA
    ]
  , bgroup       "SECP112R1"
    [ benchmark "Affine"       SECP112R1.gA
    ]
  , bgroup       "SECP112R2"
    [ benchmark "Affine"       SECP112R2.gA
    ]
  , bgroup       "SECP128R1"
    [ benchmark "Affine"       SECP128R1.gA
    ]
  , bgroup       "SECP128R2"
    [ benchmark "Affine"       SECP128R2.gA
    ]
  , bgroup       "SECP160K1"
    [ benchmark "Affine"       SECP160K1.gA
    ]
  , bgroup       "SECP160R1"
    [ benchmark "Affine"       SECP160R1.gA
    ]
  , bgroup       "SECP160R2"
    [ benchmark "Affine"       SECP160R2.gA
    ]
  , bgroup       "SECP192K1"
    [ benchmark "Affine"       SECP192K1.gA
    ]
  , bgroup       "SECP192R1"
    [ benchmark "Affine"       SECP192R1.gA
    ]
  , bgroup       "SECP224K1"
    [ benchmark "Affine"       SECP224K1.gA
    ]
  , bgroup       "SECP224R1"
    [ benchmark "Affine"       SECP224R1.gA
    ]
  , bgroup       "SECP256K1"
    [ benchmark "Affine"       SECP256K1.gA
    ]
  , bgroup       "SECP256R1"
    [ benchmark "Affine"       SECP256R1.gA
    ]
  , bgroup       "SECP384R1"
    [ benchmark "Affine"       SECP384R1.gA
    ]
  , bgroup       "SECP521R1"
    [ benchmark "Affine"       SECP521R1.gA
    ]
  ]
