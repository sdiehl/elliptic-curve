module Generate.Montgomery.Parameters
  ( curves
  ) where

import Protolude

import Generate.Montgomery.Types

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

curves :: [Curve]
curves =
  [ curve448
  , curve25519
  , curve383187
  , m221
  , m383
  , m511
  ]

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

curve448 :: Curve
curve448 = Curve
  { types = Types
    { curve   = "Curve448"
    , field   = PrimeField "Fp" 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x262a6
    , b = PF 0x1
    , x = PF 0x5
    , y = PF 0x7d235d1295f5b1f66c98ab6e58326fcecbae5d34f55545d060f75dc28df3f6edb8027e2346430d211312c4b150677af76fd7223d457b5b1a
    , h = 0x4
    , n = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
    , p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    }
  }

curve25519 :: Curve
curve25519 = Curve
  { types = Types
    { curve   = "Curve25519"
    , field   = PrimeField "Fp" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x76d06
    , b = PF 0x1
    , x = PF 0x9
    , y = PF 0x20ae19a1b8a086b4e01edd2c7748d14c923d4d7e6d7c61b229e9c5a27eced3d9
    , h = 0x8
    , n = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
    , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
    }
  }

curve383187 :: Curve
curve383187 = Curve
  { types = Types
    { curve   = "Curve383187"
    , field   = PrimeField "Fp" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x38251
    , b = PF 0x1
    , x = PF 0x5
    , y = PF 0x1eebe07dc1871896732b12d5504a32370471965c7a11f2c89865f855ab3cbd7c224e3620c31af3370788457dd5ce46df
    , h = 0x8
    , n = 0x1000000000000000000000000000000000000000000000000e85a85287a1488acd41ae84b2b7030446f72088b00a0e21
    , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    }
  }

m221 :: Curve
m221 = Curve
  { types = Types
    { curve   = "M221"
    , field   = PrimeField "Fp" 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1c93a
    , b = PF 0x1
    , x = PF 0x4
    , y = PF 0xf7acdd2a4939571d1cef14eca37c228e61dbff10707dc6c08c5056d
    , h = 0x8
    , n = 0x40000000000000000000000000015a08ed730e8a2f77f005042605b
    , p = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
    }
  }

m383 :: Curve
m383 = Curve
  { types = Types
    { curve   = "M383"
    , field   = PrimeField "Fp" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1f82fe
    , b = PF 0x1
    , x = PF 0xc
    , y = PF 0x1ec7ed04aaf834af310e304b2da0f328e7c165f0e8988abd3992861290f617aa1f1b2e7d0b6e332e969991b62555e77e
    , h = 0x8
    , n = 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7
    , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    }
  }

m511 :: Curve
m511 = Curve
  { types = Types
    { curve   = "M511"
    , field   = PrimeField "Fp" 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x81806
    , b = PF 0x1
    , x = PF 0x5
    , y = PF 0x2fbdc0ad8530803d28fdbad354bb488d32399ac1cf8f6e01ee3f96389b90c809422b9429e8a43dbf49308ac4455940abe9f1dbca542093a895e30a64af056fa5
    , h = 0x8
    , n = 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b
    , p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
    }
  }
