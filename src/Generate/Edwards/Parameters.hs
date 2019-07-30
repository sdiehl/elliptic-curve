module Generate.Edwards.Parameters
  ( curves
  ) where

import Protolude

import Generate.Edwards.Types

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

curves :: [Curve]
curves =
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
  { types = Types
    { curve   = "Curve1174"
    , field   = PrimeField "Fq" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
    , field'  = PrimeField "Fr" 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb61
    , h = 0x4
    , q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
    , r = 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
    }
  , affine = Affine
    { xA = PF 0x37fbb0cea308c479343aee7c029a190c021d96a492ecd6516123f27bce29eda
    , yA = PF 0x6b72f82d47fb7cc6656841169840e0c4fe2dee2af3f976ba4ccb1bf9b46360e
    }
  }

curve41417 :: Curve
curve41417 = Curve
  { types = Types
    { curve   = "Curve41417"
    , field   = PrimeField "Fq" 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
    , field'  = PrimeField "Fr" 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0xe21
    , h = 0x8
    , q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
    , r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
    }
  , affine = Affine
    { xA = PF 0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
    , yA = PF 0x22
    }
  }

e222 :: Curve
e222 = Curve
  { types = Types
    { curve   = "E222"
    , field   = PrimeField "Fq" 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
    , field'  = PrimeField "Fr" 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0x27166
    , h = 0x4
    , q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
    , r = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
    }
  , affine = Affine
    { xA = PF 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
    , yA = PF 0x1c
    }
  }

e382 :: Curve
e382 = Curve
  { types = Types
    { curve   = "E382"
    , field   = PrimeField "Fq" 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
    , field'  = PrimeField "Fr" 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
    , h = 0x4
    , q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
    , r = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
    }
  , affine = Affine
    { xA = PF 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
    , yA = PF 0x11
    }
  }

e521 :: Curve
e521 = Curve
  { types = Types
    { curve   = "E521"
    , field   = PrimeField "Fq" 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
    , h = 0x4
    , q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
    }
  , affine = Affine
    { xA = PF 0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
    , yA = PF 0xc
    }
  }

ed448 :: Curve
ed448 = Curve
  { types = Types
    { curve   = "Ed448"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffff6756
    , h = 0x4
    , q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , r = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
    }
  , affine = Affine
    { xA = PF 0x297ea0ea2692ff1b4faff46098453a6a26adf733245f065c3c59d0709cecfa96147eaaf3932d94c63d96c170033f4ba0c7f0de840aed939f
    , yA = PF 0x13
    }
  }

ed3363 :: Curve
ed3363 = Curve
  { types = Types
    { curve   = "Ed3363"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
    , field'  = PrimeField "Fr" 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1
    , d = PF 0x2b67
    , h = 0x8
    , q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
    , r = 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805
    }
  , affine = Affine
    { xA = PF 0xc
    , yA = PF 0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
    }
  }

ed25519 :: Curve
ed25519 = Curve
  { types = Types
    { curve   = "Ed25519"
    , field   = PrimeField "Fq" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
    , field'  = PrimeField "Fr" 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
    , d = PF 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
    , h = 0x8
    , q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
    , r = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
    }
  , affine = Affine
    { xA = PF 0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a
    , yA = PF 0x6666666666666666666666666666666666666666666666666666666666666658
    }
  }

jubjub :: Curve
jubjub = Curve
  { types = Types
    { curve   = "JubJub"
    , field   = PrimeField "Fq" 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    , field'  = PrimeField "Fr" 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000000
    , d = PF 0x2a9318e74bfa2b48f5fd9207e6bd7fd4292d7f6d37579d2601065fd6d6343eb1
    , h = 0x8
    , q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    , r = 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
    }
  , affine = Affine
    { xA = PF 0x5183972af8eff38ca624b4df00384882000c546bf2f39ede7f4ecf1a74f976c4
    , yA = PF 0x3b43f8472ca2fc2c9e8fcc5abd9dc308096c8707ffa6833b146bad709349702e
    }
  }
