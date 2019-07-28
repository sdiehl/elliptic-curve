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
  [ benchmark       "Anomalous"       Anomalous._g
  , benchmark   "ANSSIFRP256V1"   ANSSIFRP256V1._g
  , benchmark        "BLS12381"        BLS12381._g
  , benchmark       "BLS12381T"       BLS12381T._g
  , benchmark        "BLS48581"        BLS48581._g
  , benchmark       "BLS48581T"       BLS48581T._g
  , benchmark           "BN224"           BN224._g
  , benchmark           "BN254"           BN254._g
  , benchmark          "BN254T"          BN254T._g
  , benchmark          "BN254A"          BN254A._g
  , benchmark         "BN254AT"         BN254AT._g
  , benchmark          "BN254B"          BN254B._g
  , benchmark         "BN254BT"         BN254BT._g
  , benchmark           "BN256"           BN256._g
  , benchmark           "BN384"           BN384._g
  , benchmark           "BN462"           BN462._g
  , benchmark          "BN462T"          BN462T._g
  , benchmark           "BN512"           BN512._g
  , benchmark "BrainpoolP160R1" BrainpoolP160R1._g
  , benchmark "BrainpoolP160T1" BrainpoolP160T1._g
  , benchmark "BrainpoolP192R1" BrainpoolP192R1._g
  , benchmark "BrainpoolP192T1" BrainpoolP192T1._g
  , benchmark "BrainpoolP224R1" BrainpoolP224R1._g
  , benchmark "BrainpoolP224T1" BrainpoolP224T1._g
  , benchmark "BrainpoolP256R1" BrainpoolP256R1._g
  , benchmark "BrainpoolP256T1" BrainpoolP256T1._g
  , benchmark "BrainpoolP320R1" BrainpoolP320R1._g
  , benchmark "BrainpoolP320T1" BrainpoolP320T1._g
  , benchmark "BrainpoolP384R1" BrainpoolP384R1._g
  , benchmark "BrainpoolP384T1" BrainpoolP384T1._g
  , benchmark "BrainpoolP512R1" BrainpoolP512R1._g
  , benchmark "BrainpoolP512T1" BrainpoolP512T1._g
  , benchmark       "SECP112R1"       SECP112R1._g
  , benchmark       "SECP112R2"       SECP112R2._g
  , benchmark       "SECP128R1"       SECP128R1._g
  , benchmark       "SECP128R2"       SECP128R2._g
  , benchmark       "SECP160K1"       SECP160K1._g
  , benchmark       "SECP160R1"       SECP160R1._g
  , benchmark       "SECP160R2"       SECP160R2._g
  , benchmark       "SECP192K1"       SECP192K1._g
  , benchmark       "SECP192R1"       SECP192R1._g
  , benchmark       "SECP224K1"       SECP224K1._g
  , benchmark       "SECP224R1"       SECP224R1._g
  , benchmark       "SECP256K1"       SECP256K1._g
  , benchmark       "SECP256R1"       SECP256R1._g
  , benchmark       "SECP384R1"       SECP384R1._g
  , benchmark       "SECP521R1"       SECP521R1._g
  ]
