module Generate.Curve.Edwards
  ( edwardsCurves
  ) where

import Generate.Type (Curve(..), Element(..))

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

edwardsCurves :: [Curve]
edwardsCurves =
  [ curve1174
  , curve41417
  , e222
  , e382
  , e521
  , ed448
  , ed3363
  , ed25519
  , jubjub
  ]

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

curve1174 :: Curve
curve1174 = Curve
  { i = "Curve1174"
  , a = PF 0x1
  , b = PF 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb61
  , x = PF 0x37fbb0cea308c479343aee7c029a190c021d96a492ecd6516123f27bce29eda
  , y = PF 0x6b72f82d47fb7cc6656841169840e0c4fe2dee2af3f976ba4ccb1bf9b46360e
  , h = 0x4
  , n = 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
  , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
  }

curve41417 :: Curve
curve41417 = Curve
  { i = "Curve41417"
  , a = PF 0x1
  , b = PF 0xe21
  , x = PF 0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
  , y = PF 0x22
  , h = 0x8
  , n = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
  , p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
  }

e222 :: Curve
e222 = Curve
  { i = "E222"
  , a = PF 0x1
  , b = PF 0x27166
  , x = PF 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
  , y = PF 0x1c
  , h = 0x4
  , n = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
  , p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
  }

e382 :: Curve
e382 = Curve
  { i = "E382"
  , a = PF 0x1
  , b = PF 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
  , x = PF 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
  , y = PF 0x11
  , h = 0x4
  , n = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
  , p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
  }

e521 :: Curve
e521 = Curve
  { i = "E521"
  , a = PF 0x1
  , b = PF 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
  , x = PF 0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
  , y = PF 0xc
  , h = 0x4
  , n = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
  , p = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
  }

ed448 :: Curve
ed448 = Curve
  { i = "Ed448"
  , a = PF 0x1
  , b = PF 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffff6756
  , x = PF 0x297ea0ea2692ff1b4faff46098453a6a26adf733245f065c3c59d0709cecfa96147eaaf3932d94c63d96c170033f4ba0c7f0de840aed939f
  , y = PF 0x13
  , h = 0x4
  , n = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
  , p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
  }

ed3363 :: Curve
ed3363 = Curve
  { i = "Ed3363"
  , a = PF 0x1
  , b = PF 0x2b67
  , x = PF 0xc
  , y = PF 0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
  , h = 0x8
  , n = 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805
  , p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
  }

ed25519 :: Curve
ed25519 = Curve
  { i = "Ed25519"
  , a = PF 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
  , b = PF 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
  , x = PF 0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a
  , y = PF 0x6666666666666666666666666666666666666666666666666666666666666658
  , h = 0x8
  , n = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
  , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  }

jubjub :: Curve
jubjub = Curve
  { i = "JubJub"
  , a = PF 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000000
  , b = PF 0x2a9318e74bfa2b48f5fd9207e6bd7fd4292d7f6d37579d2601065fd6d6343eb1
  , x = PF 0x5183972af8eff38ca624b4df00384882000c546bf2f39ede7f4ecf1a74f976c4
  , y = PF 0x3b43f8472ca2fc2c9e8fcc5abd9dc308096c8707ffa6833b146bad709349702e
  , h = 0x8
  , n = 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
  , p = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  }
