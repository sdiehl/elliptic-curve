module Generate.Weierstrass.Parameters
  ( curves
  ) where

import Protolude

import Generate.Weierstrass.Types

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

curves :: [Curve]
curves =
  [ anomalous
  , anssifrp256v1
  , bls12381
  , bls12381t
  , bls48581
  , bls48581t
  , bn224
  , bn254
  , bn254t
  , bn254a
  , bn254at
  , bn254b
  , bn254bt
  , bn256
  , bn384
  , bn462
  , bn462t
  , bn512
  , brainpoolp160r1
  , brainpoolp160t1
  , brainpoolp192r1
  , brainpoolp192t1
  , brainpoolp224r1
  , brainpoolp224t1
  , brainpoolp256r1
  , brainpoolp256t1
  , brainpoolp320r1
  , brainpoolp320t1
  , brainpoolp384r1
  , brainpoolp384t1
  , brainpoolp512r1
  , brainpoolp512t1
  , secp112r1
  , secp112r2
  , secp128r1
  , secp128r2
  , secp160k1
  , secp160r1
  , secp160r2
  , secp192k1
  , secp192r1
  , secp224k1
  , secp224r1
  , secp256k1
  , secp256r1
  , secp384r1
  , secp521r1
  ]

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

anomalous :: Curve
anomalous = Curve
  { types = Types
    { curve   = "Anomalous"
    , field   = PrimeField "Fq" 0xb0000000000000000000000953000000000000000000001f9d7
    , field'  = PrimeField "Fr" 0xb0000000000000000000000953000000000000000000001f9d7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x98d0fac687d6343eb1a1f595283eb1a1f58d0fac687d635f5e4
    , b = PF 0x4a1f58d0fac687d6343eb1a5e2d6343eb1a1f58d0fac688ab3f
    , h = 0x1
    , q = 0xb0000000000000000000000953000000000000000000001f9d7
    , r = 0xb0000000000000000000000953000000000000000000001f9d7
    }
  , affine = Affine
    { xA = PF 0x101efb35fd1963c4871a2d17edaafa7e249807f58f8705126c6
    , yA = PF 0x22389a3954375834304ba1d509a97de6c07148ea7f5951b20e7
    }
  }

anssifrp256v1 :: Curve
anssifrp256v1 = Curve
  { types = Types
    { curve   = "ANSSIFRP256V1"
    , field   = PrimeField "Fq" 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
    , field'  = PrimeField "Fr" 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c00
    , b = PF 0xee353fca5428a9300d4aba754a44c00fdfec0c9ae4b1a1803075ed967b7bb73f
    , h = 0x1
    , q = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
    , r = 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1
    }
  , affine = Affine
    { xA = PF 0xb6b3d4c356c139eb31183d4749d423958c27d2dcaf98b70164c97a2dd98f5cff
    , yA = PF 0x6142e0f7c8b204911f9271f0f3ecef8c2701c307e8e4c9e183115a1554062cfb
    }
  }

bls12381 :: Curve
bls12381 = Curve
  { types = Types
    { curve   = "BLS12381"
    , field   = PrimeField "Fq" 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
    , field'  = PrimeField "Fr" 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x4
    , h = 0x396c8c005555e1568c00aaab0000aaab
    , q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
    , r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    }
  , affine = Affine
    { xA = PF 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
    , yA = PF 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1
    }
  }

bls12381t :: Curve
bls12381t = Curve
  { types = Types
    { curve   = "BLS12381T"
    , field   = ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 1" Nothing
    , field'  = PrimeField "Fr" 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    , imports = Just "import Curve.Weierstrass.BLS12381 (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ PF 0x4
             , PF 0x4
             ]
    , h = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5
    , q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
    , r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    }
  , affine = Affine
    { xA = EF [ PF 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
              , PF 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
              ]
    , yA = EF [ PF 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
              , PF 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be
              ]
    }
  }

bls48581 :: Curve
bls48581 = Curve
  { types = Types
    { curve   = "BLS48581"
    , field   = PrimeField "Fq" 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
    , field'  = PrimeField "Fr" 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x1
    , h = 0x85555841aaaec4ac
    , q = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
    , r = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
    }
  , affine = Affine
    { xA = PF 0x2af59b7ac340f2baf2b73df1e93f860de3f257e0e86868cf61abdbaedffb9f7544550546a9df6f9645847665d859236ebdbc57db368b11786cb74da5d3a1e6d8c3bce8732315af640
    , yA = PF 0xcefda44f6531f91f86b3a2d1fb398a488a553c9efeb8a52e991279dd41b720ef7bb7beffb98aee53e80f678584c3ef22f487f77c2876d1b2e35f37aef7b926b576dbb5de3e2587a70
    }
  }

bls48581t :: Curve
bls48581t = Curve
  { types = Types
    { curve   = "BLS48581T"
    , field   = ExtensionField "Fq8" "Fq4" "PolynomialW" "x * x + t x" (Just
                ( ExtensionField "Fq4" "Fq2" "PolynomialV" "x * x + 1 + t x" (Just
                  ( ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 1" Nothing
                  ))
                ))
    , field'  = PrimeField "Fr" 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
    , imports = Just "import Curve.Weierstrass.BLS48581 (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ EF [
                  ]
             , EF [ EF [
                       ]
                  , EF [ PF 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c895
                       , PF 0x9407b9ff9a3b7989c12718ea38095002b7427c6891098dd9df36078f9cbaa225245721d7b7041566ce6981ca7a39b6d7b41b3d2a898b877052bc7efb90d2524561f6e0aa732b2c896
                       ]
                  ]
             ]
    , h = 0x170e915cb0a6b7406b8d94042317f811d6bc3fc6e211ada42e58ccfcb3ac076a7e4499d700a0c23dc4b0c078f92def8c87b7fe63e1eea270db353a4ef4d38b5998ad8f0d042ea24c8f02be1c0c83992fe5d7725227bb27123a949e0876c0a8ce0a67326db0e955dcb791b867f31d6bfa62fbdd5f44a00504df04e186fae033f1eb43c1b1a08b6e086eff03c8fee9ebdd1e191a8a4b0466c90b389987de5637d5dd13dab33196bd2e5afa6cd19cf0fc3fc7db7ece1f3fac742626b1b02fcee04043b2ea96492f6afa51739597c54bb78aa6b0b99319fef9d09f768831018ee6564c68d054c62f2e0b4549426fec24ab26957a669dba2a2b6945ce40c9aec6afdeda16c79e15546cd7771fa544d5364236690ea06832679562a68731420ae52d0d35a90b8d10b688e31b6aee45f45b7a5083c71732105852decc888f64839a4de33b99521f0984a418d20fc7b0609530e454f0696fa2a8075ac01cc8ae3869e8d0fe1f3788ffac4c01aa2720e431da333c83d9663bfb1fb7a1a7b90528482c6be7892299030bb51a51dc7e91e9156874416bf4c26f1ea7ec578058563960ef92bbbb8632d3a1b695f954af10e9a78e40acffc13b06540aae9da5287fc4429485d44e6289d8c0d6a3eb2ece35012452751839fb48bc14b515478e2ff412d930ac20307561f3a5c998e6bcbfebd97effc6433033a2361bfcdc4fc74ad379a16c6dea49c209b1
    , q = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
    , r = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
    }
  , affine = Affine
    { xA = EF [ EF [ EF [ PF 0x5d615d9a7871e4a38237fa45a2775debabbefc70344dbccb7de64db3a2ef156c46ff79baad1a8c42281a63ca0612f400503004d80491f510317b79766322154dec34fd0b4ace8bfab
                        , PF 0x7c4973ece2258512069b0e86abc07e8b22bb6d980e1623e9526f6da12307f4e1c3943a00abfedf16214a76affa62504f0c3c7630d979630ffd75556a01afa143f1669b36676b47c57
                        ]
                   , EF [ PF 0x1fccc70198f1334e1b2ea1853ad83bc73a8a6ca9ae237ca7a6d6957ccbab5ab6860161c1dbd19242ffae766f0d2a6d55f028cbdfbb879d5fea8ef4cded6b3f0b46488156ca55a3e6a
                        , PF 0xbe2218c25ceb6185c78d8012954d4bfe8f5985ac62f3e5821b7b92a393f8be0cc218a95f63e1c776e6ec143b1b279b9468c31c5257c200ca52310b8cb4e80bc3f09a7033cbb7feafe
                        ]
                   ]
              , EF [ EF [ PF 0x38b91c600b35913a3c598e4caa9dd63007c675d0b1642b5675ff0e7c5805386699981f9e48199d5ac10b2ef492ae589274fad55fc1889aa80c65b5f746c9d4cbb739c3a1c53f8cce5
                        , PF 0xc96c7797eb0738603f1311e4ecda088f7b8f35dcef0977a3d1a58677bb037418181df63835d28997eb57b40b9c0b15dd7595a9f177612f097fc7960910fce3370f2004d914a3c093a
                        ]
                   , EF [ PF 0xb9b7951c6061ee3f0197a498908aee660dea41b39d13852b6db908ba2c0b7a449cef11f293b13ced0fd0caa5efcf3432aad1cbe4324c22d63334b5b0e205c3354e41607e60750e057
                        , PF 0x827d5c22fb2bdec5282624c4f4aaa2b1e5d7a9defaf47b5211cf741719728a7f9f8cfca93f29cff364a7190b7e2b0d4585479bd6aebf9fc44e56af2fc9e97c3f84e19da00fbc6ae34
                        ]
                   ]
              ]
    , yA = EF [ EF [ EF [ PF 0xeb53356c375b5dfa497216452f3024b918b4238059a577e6f3b39ebfc435faab0906235afa27748d90f7336d8ae5163c1599abf77eea6d659045012ab12c0ff323edd3fe4d2d7971
                        , PF 0x284dc75979e0ff144da6531815fcadc2b75a422ba325e6fba01d72964732fcbf3afb096b243b1f192c5c3d1892ab24e1dd212fa097d760e2e588b423525ffc7b111471db936cd5665
                        ]
                   , EF [ PF 0xb36a201dd008523e421efb70367669ef2c2fc5030216d5b119d3a480d370514475f7d5c99d0e90411515536ca3295e5e2f0c1d35d51a652269cbc7c46fc3b8fde68332a526a2a8474
                        , PF 0xaec25a4621edc0688223fbbd478762b1c2cded3360dcee23dd8b0e710e122d2742c89b224333fa40dced2817742770ba10d67bda503ee5e578fb3d8b8a1e5337316213da92841589d
                        ]
                   ]
              , EF [ EF [ PF 0xd209d5a223a9c46916503fa5a88325a2554dc541b43dd93b5a959805f1129857ed85c77fa238cdce8a1e2ca4e512b64f59f430135945d137b08857fdddfcf7a43f47831f982e50137
                        , PF 0x7d0d03745736b7a513d339d5ad537b90421ad66eb16722b589d82e2055ab7504fa83420e8c270841f6824f47c180d139e3aafc198caa72b679da59ed8226cf3a594eedc58cf90bee4
                        ]
                   , EF [ PF 0x896767811be65ea25c2d05dfdd17af8a006f364fc0841b064155f14e4c819a6df98f425ae3a2864f22c1fab8c74b2618b5bb40fa639f53dccc9e884017d9aa62b3d41faeafeb23986
                        , PF 0x35e2524ff89029d393a5c07e84f981b5e068f1406be8e50c87549b6ef8eca9a9533a3f8e69c31e97e1ad0333ec719205417300d8c4ab33f748e5ac66e84069c55d667ffcb732718b6
                        ]
                   ]
              ]
    }
  }

bn224 :: Curve
bn224 = Curve
  { types = Types
    { curve   = "BN224"
    , field   = PrimeField "Fq" 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833
    , field'  = PrimeField "Fr" 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833
    , r = 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0x2
    }
  }

bn254 :: Curve
bn254 = Curve
  { types = Types
    { curve   = "BN254"
    , field   = PrimeField "Fq" 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
    , field'  = PrimeField "Fr" 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
    , r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0x2
    }
  }

bn254t :: Curve
bn254t = Curve
  { types = Types
    { curve   = "BN254T"
    , field   = ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 1" Nothing
    , field'  = PrimeField "Fr" 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
    , imports = Just "import Curve.Weierstrass.BN254 (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ PF 0x2b149d40ceb8aaae81be18991be06ac3b5b4c5e559dbefa33267e6dc24a138e5
             , PF 0x9713b03af0fed4cd2cafadeed8fdf4a74fa084e52d1852e4a2bd0685c315d2
             ]
    , h = 0x30644e72e131a029b85045b68181585e06ceecda572a2489345f2299c0f9fa8d
    , q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
    , r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
    }
  , affine = Affine
    { xA = EF [ PF 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
              , PF 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
              ]
    , yA = EF [ PF 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
              , PF 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
              ]
    }
  }

bn254a :: Curve
bn254a = Curve
  { types = Types
    { curve   = "BN254A"
    , field   = PrimeField "Fq" 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
    , field'  = PrimeField "Fr" 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x5
    , h = 0x1
    , q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
    , r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
    }
  }

bn254at :: Curve
bn254at = Curve
  { types = Types
    { curve   = "BN254AT"
    , field   = ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 5" Nothing
    , field'  = PrimeField "Fr" 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
    , imports = Just "import Curve.Weierstrass.BN254A (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ PF 0x0
             , PF 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000000
             ]
    , h = 0x2370fb049d410fbe4e761a9886e50241dc42cf101e0000017e80600000000001
    , q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
    , r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
    }
  , affine = Affine
    { xA = EF [ PF 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
              , PF 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
              ]
    , yA = EF [ PF 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
              , PF 0xee97d6de9902a27d00e952232a78700863bc9aa9be960c32f5bf9fd0a32d345
              ]
    }
  }

bn254b :: Curve
bn254b = Curve
  { types = Types
    { curve   = "BN254B"
    , field   = PrimeField "Fq" 0x2523648240000001ba344d80000000086121000000000013a700000000000013
    , field'  = PrimeField "Fr" 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x2
    , h = 0x1
    , q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
    , r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
    }
  , affine = Affine
    { xA = PF 0x2523648240000001ba344d80000000086121000000000013a700000000000012
    , yA = PF 0x1
    }
  }

bn254bt :: Curve
bn254bt = Curve
  { types = Types
    { curve   = "BN254BT"
    , field   = ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 1" Nothing
    , field'  = PrimeField "Fr" 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
    , imports = Just "import Curve.Weierstrass.BN254B (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ PF 0x1
             , PF 0x2523648240000001ba344d80000000086121000000000013a700000000000012
             ]
    , h = 0x2523648240000001ba344d8000000008c2a2800000000016ad00000000000019
    , q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
    , r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
    }
  , affine = Affine
    { xA = EF [ PF 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
              , PF 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
              ]
    , yA = EF [ PF 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
              , PF 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
              ]
    }
  }

bn256 :: Curve
bn256 = Curve
  { types = Types
    { curve   = "BN256"
    , field   = PrimeField "Fq" 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
    , field'  = PrimeField "Fr" 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
    , r = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0x2
    }
  }

bn384 :: Curve
bn384 = Curve
  { types = Types
    { curve   = "BN384"
    , field   = PrimeField "Fq" 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
    , field'  = PrimeField "Fr" 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
    , r = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0x2
    }
  }

bn462 :: Curve
bn462 = Curve
  { types = Types
    { curve   = "BN462"
    , field   = PrimeField "Fq" 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
    , field'  = PrimeField "Fr" 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x5
    , h = 0x1
    , q = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
    , r = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
    }
  , affine = Affine
    { xA = PF 0x21a6d67ef250191fadba34a0a30160b9ac9264b6f95f63b3edbec3cf4b2e689db1bbb4e69a416a0b1e79239c0372e5cd70113c98d91f36b6980d
    , yA = PF 0x118ea0460f7f7abb82b33676a7432a490eeda842cccfa7d788c659650426e6af77df11b8ae40eb80f475432c66600622ecaa8a5734d36fb03de
    }
  }

bn462t :: Curve
bn462t = Curve
  { types = Types
    { curve   = "BN462T"
    , field   = ExtensionField "Fq2" "Fq" "PolynomialU" "x * x + 1" Nothing
    , field'  = PrimeField "Fr" 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
    , imports = Just "import Curve.Weierstrass.BN462 (Fq)"
    }
  , parameters = Parameters
    { a = EF [
             ]
    , b = EF [ PF 0x2
             , PF 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138012
             ]
    , h = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908fa1ce0227fffffffff6ff66fc63f5f7f4c0000000002401b008a0168019
    , q = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
    , r = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
    }
  , affine = Affine
    { xA = EF [ PF 0x257ccc85b58dda0dfb38e3a8cbdc5482e0337e7c1cd96ed61c913820408208f9ad2699bad92e0032ae1f0aa6a8b48807695468e3d934ae1e4df
              , PF 0x1d2e4343e8599102af8edca849566ba3c98e2a354730cbed9176884058b18134dd86bae555b783718f50af8b59bf7e850e9b73108ba6aa8cd283
              ]
    , yA = EF [ PF 0xa0650439da22c1979517427a20809eca035634706e23c3fa7a6bb42fe810f1399a1f41c9ddae32e03695a140e7b11d7c3376e5b68df0db7154e
              , PF 0x73ef0cbd438cbe0172c8ae37306324d44d5e6b0c69ac57b393f1ab370fd725cc647692444a04ef87387aa68d53743493b9eba14cc552ca2a93a
              ]
    }
  }

bn512 :: Curve
bn512 = Curve
  { types = Types
    { curve   = "BN512"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33
    , field'  = PrimeField "Fr" 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33
    , r = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed
    }
  , affine = Affine
    { xA = PF 0x1
    , yA = PF 0x2
    }
  }

brainpoolp160r1 :: Curve
brainpoolp160r1 = Curve
  { types = Types
    { curve   = "BrainpoolP160R1"
    , field   = PrimeField "Fq" 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
    , field'  = PrimeField "Fr" 0xe95e4a5f737059dc60df5991d45029409e60fc09
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x340e7be2a280eb74e2be61bada745d97e8f7c300
    , b = PF 0x1e589a8595423412134faa2dbdec95c8d8675e58
    , h = 0x1
    , q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
    , r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
    }
  , affine = Affine
    { xA = PF 0xbed5af16ea3f6a4f62938c4631eb5af7bdbcdbc3
    , yA = PF 0x1667cb477a1a8ec338f94741669c976316da6321
    }
  }

brainpoolp160t1 :: Curve
brainpoolp160t1 = Curve
  { types = Types
    { curve   = "BrainpoolP160T1"
    , field   = PrimeField "Fq" 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
    , field'  = PrimeField "Fr" 0xe95e4a5f737059dc60df5991d45029409e60fc09
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
    , b = PF 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
    , h = 0x1
    , q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
    , r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
    }
  , affine = Affine
    { xA = PF 0xb199b13b9b34efc1397e64baeb05acc265ff2378
    , yA = PF 0xadd6718b7c7c1961f0991b842443772152c9e0ad
    }
  }

brainpoolp192r1 :: Curve
brainpoolp192r1 = Curve
  { types = Types
    { curve   = "BrainpoolP192R1"
    , field   = PrimeField "Fq" 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
    , field'  = PrimeField "Fr" 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x6a91174076b1e0e19c39c031fe8685c1cae040e5c69a28ef
    , b = PF 0x469a28ef7c28cca3dc721d044f4496bcca7ef4146fbf25c9
    , h = 0x1
    , q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
    , r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
    }
  , affine = Affine
    { xA = PF 0xc0a0647eaab6a48753b033c56cb0f0900a2f5c4853375fd6
    , yA = PF 0x14b690866abd5bb88b5f4828c1490002e6773fa2fa299b8f
    }
  }

brainpoolp192t1 :: Curve
brainpoolp192t1 = Curve
  { types = Types
    { curve   = "BrainpoolP192T1"
    , field   = PrimeField "Fq" 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
    , field'  = PrimeField "Fr" 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xc302f41d932a36cda7a3463093d18db78fce476de1a86294
    , b = PF 0x13d56ffaec78681e68f9deb43b35bec2fb68542e27897b79
    , h = 0x1
    , q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
    , r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
    }
  , affine = Affine
    { xA = PF 0x3ae9e58c82f63c30282e1fe7bbf43fa72c446af6f4618129
    , yA = PF 0x97e2c5667c2223a902ab5ca449d0084b7e5b3de7ccc01c9
    }
  }

brainpoolp224r1 :: Curve
brainpoolp224r1 = Curve
  { types = Types
    { curve   = "BrainpoolP224R1"
    , field   = PrimeField "Fq" 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
    , field'  = PrimeField "Fr" 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x68a5e62ca9ce6c1c299803a6c1530b514e182ad8b0042a59cad29f43
    , b = PF 0x2580f63ccfe44138870713b1a92369e33e2135d266dbb372386c400b
    , h = 0x1
    , q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
    , r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
    }
  , affine = Affine
    { xA = PF 0xd9029ad2c7e5cf4340823b2a87dc68c9e4ce3174c1e6efdee12c07d
    , yA = PF 0x58aa56f772c0726f24c6b89e4ecdac24354b9e99caa3f6d3761402cd
    }
  }

brainpoolp224t1 :: Curve
brainpoolp224t1 = Curve
  { types = Types
    { curve   = "BrainpoolP224T1"
    , field   = PrimeField "Fq" 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
    , field'  = PrimeField "Fr" 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0fc
    , b = PF 0x4b337d934104cd7bef271bf60ced1ed20da14c08b3bb64f18a60888d
    , h = 0x1
    , q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
    , r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
    }
  , affine = Affine
    { xA = PF 0x6ab1e344ce25ff3896424e7ffe14762ecb49f8928ac0c76029b4d580
    , yA = PF 0x374e9f5143e568cd23f3f4d7c0d4b1e41c8cc0d1c6abd5f1a46db4c
    }
  }

brainpoolp256r1 :: Curve
brainpoolp256r1 = Curve
  { types = Types
    { curve   = "BrainpoolP256R1"
    , field   = PrimeField "Fq" 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
    , field'  = PrimeField "Fr" 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x7d5a0975fc2c3057eef67530417affe7fb8055c126dc5c6ce94a4b44f330b5d9
    , b = PF 0x26dc5c6ce94a4b44f330b5d9bbd77cbf958416295cf7e1ce6bccdc18ff8c07b6
    , h = 0x1
    , q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
    , r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
    }
  , affine = Affine
    { xA = PF 0x8bd2aeb9cb7e57cb2c4b482ffc81b7afb9de27e1e3bd23c23a4453bd9ace3262
    , yA = PF 0x547ef835c3dac4fd97f8461a14611dc9c27745132ded8e545c1d54c72f046997
    }
  }

brainpoolp256t1 :: Curve
brainpoolp256t1 = Curve
  { types = Types
    { curve   = "BrainpoolP256T1"
    , field   = PrimeField "Fq" 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
    , field'  = PrimeField "Fr" 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5374
    , b = PF 0x662c61c430d84ea4fe66a7733d0b76b7bf93ebc4af2f49256ae58101fee92b04
    , h = 0x1
    , q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
    , r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
    }
  , affine = Affine
    { xA = PF 0xa3e8eb3cc1cfe7b7732213b23a656149afa142c47aafbc2b79a191562e1305f4
    , yA = PF 0x2d996c823439c56d7f7b22e14644417e69bcb6de39d027001dabe8f35b25c9be
    }
  }

brainpoolp320r1 :: Curve
brainpoolp320r1 = Curve
  { types = Types
    { curve   = "BrainpoolP320R1"
    , field   = PrimeField "Fq" 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
    , field'  = PrimeField "Fr" 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x3ee30b568fbab0f883ccebd46d3f3bb8a2a73513f5eb79da66190eb085ffa9f492f375a97d860eb4
    , b = PF 0x520883949dfdbc42d3ad198640688a6fe13f41349554b49acc31dccd884539816f5eb4ac8fb1f1a6
    , h = 0x1
    , q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
    , r = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
    }
  , affine = Affine
    { xA = PF 0x43bd7e9afb53d8b85289bcc48ee5bfe6f20137d10a087eb6e7871e2a10a599c710af8d0d39e20611
    , yA = PF 0x14fdd05545ec1cc8ab4093247f77275e0743ffed117182eaa9c77877aaac6ac7d35245d1692e8ee1
    }
  }

brainpoolp320t1 :: Curve
brainpoolp320t1 = Curve
  { types = Types
    { curve   = "BrainpoolP320T1"
    , field   = PrimeField "Fq" 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
    , field'  = PrimeField "Fr" 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e24
    , b = PF 0xa7f561e038eb1ed560b3d147db782013064c19f27ed27c6780aaf77fb8a547ceb5b4fef422340353
    , h = 0x1
    , q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
    , r = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
    }
  , affine = Affine
    { xA = PF 0x925be9fb01afc6fb4d3e7d4990010f813408ab106c4f09cb7ee07868cc136fff3357f624a21bed52
    , yA = PF 0x63ba3a7a27483ebf6671dbef7abb30ebee084e58a0b077ad42a5a0989d1ee71b1b9bc0455fb0d2c3
    }
  }

brainpoolp384r1 :: Curve
brainpoolp384r1 = Curve
  { types = Types
    { curve   = "BrainpoolP384R1"
    , field   = PrimeField "Fq" 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
    , field'  = PrimeField "Fr" 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x7bc382c63d8c150c3c72080ace05afa0c2bea28e4fb22787139165efba91f90f8aa5814a503ad4eb04a8c7dd22ce2826
    , b = PF 0x4a8c7dd22ce28268b39b55416f0447c2fb77de107dcd2a62e880ea53eeb62d57cb4390295dbc9943ab78696fa504c11
    , h = 0x1
    , q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
    , r = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
    }
  , affine = Affine
    { xA = PF 0x1d1c64f068cf45ffa2a63a81b7c13f6b8847a3e77ef14fe3db7fcafe0cbd10e8e826e03436d646aaef87b2e247d4af1e
    , yA = PF 0x8abe1d7520f9c2a45cb1eb8e95cfd55262b70b29feec5864e19c054ff99129280e4646217791811142820341263c5315
    }
  }

brainpoolp384t1 :: Curve
brainpoolp384t1 = Curve
  { types = Types
    { curve   = "BrainpoolP384T1"
    , field   = PrimeField "Fq" 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
    , field'  = PrimeField "Fr" 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec50
    , b = PF 0x7f519eada7bda81bd826dba647910f8c4b9346ed8ccdc64e4b1abd11756dce1d2074aa263b88805ced70355a33b471ee
    , h = 0x1
    , q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
    , r = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
    }
  , affine = Affine
    { xA = PF 0x18de98b02db9a306f2afcd7235f72a819b80ab12ebd653172476fecd462aabffc4ff191b946a5f54d8d0aa2f418808cc
    , yA = PF 0x25ab056962d30651a114afd2755ad336747f93475b7a1fca3b88f2b6a208ccfe469408584dc2b2912675bf5b9e582928
    }
  }

brainpoolp512r1 :: Curve
brainpoolp512r1 = Curve
  { types = Types
    { curve   = "BrainpoolP512R1"
    , field   = PrimeField "Fq" 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
    , field'  = PrimeField "Fr" 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x7830a3318b603b89e2327145ac234cc594cbdd8d3df91610a83441caea9863bc2ded5d5aa8253aa10a2ef1c98b9ac8b57f1117a72bf2c7b9e7c1ac4d77fc94ca
    , b = PF 0x3df91610a83441caea9863bc2ded5d5aa8253aa10a2ef1c98b9ac8b57f1117a72bf2c7b9e7c1ac4d77fc94cadc083e67984050b75ebae5dd2809bd638016f723
    , h = 0x1
    , q = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
    , r = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
    }
  , affine = Affine
    { xA = PF 0x81aee4bdd82ed9645a21322e9c4c6a9385ed9f70b5d916c1b43b62eef4d0098eff3b1f78e2d0d48d50d1687b93b97d5f7c6d5047406a5e688b352209bcb9f822
    , yA = PF 0x7dde385d566332ecc0eabfa9cf7822fdf209f70024a57b1aa000c55b881f8111b2dcde494a5f485e5bca4bd88a2763aed1ca2b2fa8f0540678cd1e0f3ad80892
    }
  }

brainpoolp512t1 :: Curve
brainpoolp512t1 = Curve
  { types = Types
    { curve   = "BrainpoolP512T1"
    , field   = PrimeField "Fq" 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
    , field'  = PrimeField "Fr" 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f0
    , b = PF 0x7cbbbcf9441cfab76e1890e46884eae321f70c0bcb4981527897504bec3e36a62bcdfa2304976540f6450085f2dae145c22553b465763689180ea2571867423e
    , h = 0x1
    , q = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
    , r = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
    }
  , affine = Affine
    { xA = PF 0x640ece5c12788717b9c1ba06cbc2a6feba85842458c56dde9db1758d39c0313d82ba51735cdb3ea499aa77a7d6943a64f7a3f25fe26f06b51baa2696fa9035da
    , yA = PF 0x5b534bd595f5af0fa2c892376c84ace1bb4e3019b71634c01131159cae03cee9d9932184beef216bd71df2dadf86a627306ecff96dbb8bace198b61e00f8b332
    }
  }

secp112r1 :: Curve
secp112r1 = Curve
  { types = Types
    { curve   = "SECP112R1"
    , field   = PrimeField "Fq" 0xdb7c2abf62e35e668076bead208b
    , field'  = PrimeField "Fr" 0xdb7c2abf62e35e7628dfac6561c5
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xdb7c2abf62e35e668076bead2088
    , b = PF 0x659ef8ba043916eede8911702b22
    , h = 0x1
    , q = 0xdb7c2abf62e35e668076bead208b
    , r = 0xdb7c2abf62e35e7628dfac6561c5
    }
  , affine = Affine
    { xA = PF 0x9487239995a5ee76b55f9c2f098
    , yA = PF 0xa89ce5af8724c0a23e0e0ff77500
    }
  }

secp112r2 :: Curve
secp112r2 = Curve
  { types = Types
    { curve   = "SECP112R2"
    , field   = PrimeField "Fq" 0xdb7c2abf62e35e668076bead208b
    , field'  = PrimeField "Fr" 0x36df0aafd8b8d7597ca10520d04b
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x6127c24c05f38a0aaaf65c0ef02c
    , b = PF 0x51def1815db5ed74fcc34c85d709
    , h = 0x4
    , q = 0xdb7c2abf62e35e668076bead208b
    , r = 0x36df0aafd8b8d7597ca10520d04b
    }
  , affine = Affine
    { xA = PF 0x4ba30ab5e892b4e1649dd0928643
    , yA = PF 0xadcd46f5882e3747def36e956e97
    }
  }

secp128r1 :: Curve
secp128r1 = Curve
  { types = Types
    { curve   = "SECP128R1"
    , field   = PrimeField "Fq" 0xfffffffdffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0xfffffffe0000000075a30d1b9038a115
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xfffffffdfffffffffffffffffffffffc
    , b = PF 0xe87579c11079f43dd824993c2cee5ed3
    , h = 0x1
    , q = 0xfffffffdffffffffffffffffffffffff
    , r = 0xfffffffe0000000075a30d1b9038a115
    }
  , affine = Affine
    { xA = PF 0x161ff7528b899b2d0c28607ca52c5b86
    , yA = PF 0xcf5ac8395bafeb13c02da292dded7a83
    }
  }

secp128r2 :: Curve
secp128r2 = Curve
  { types = Types
    { curve   = "SECP128R2"
    , field   = PrimeField "Fq" 0xfffffffdffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0x3fffffff7fffffffbe0024720613b5a3
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xd6031998d1b3bbfebf59cc9bbff9aee1
    , b = PF 0x5eeefca380d02919dc2c6558bb6d8a5d
    , h = 0x4
    , q = 0xfffffffdffffffffffffffffffffffff
    , r = 0x3fffffff7fffffffbe0024720613b5a3
    }
  , affine = Affine
    { xA = PF 0x7b6aa5d85e572983e6fb32a7cdebc140
    , yA = PF 0x27b6916a894d3aee7106fe805fc34b44
    }
  }

secp160k1 :: Curve
secp160k1 = Curve
  { types = Types
    { curve   = "SECP160K1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffeffffac73
    , field'  = PrimeField "Fr" 0x100000000000000000001b8fa16dfab9aca16b6b3
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x7
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffeffffac73
    , r = 0x100000000000000000001b8fa16dfab9aca16b6b3
    }
  , affine = Affine
    { xA = PF 0x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
    , yA = PF 0x938cf935318fdced6bc28286531733c3f03c4fee
    }
  }

secp160r1 :: Curve
secp160r1 = Curve
  { types = Types
    { curve   = "SECP160R1"
    , field   = PrimeField "Fq" 0xffffffffffffffffffffffffffffffff7fffffff
    , field'  = PrimeField "Fr" 0x100000000000000000001f4c8f927aed3ca752257
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xffffffffffffffffffffffffffffffff7ffffffc
    , b = PF 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
    , h = 0x1
    , q = 0xffffffffffffffffffffffffffffffff7fffffff
    , r = 0x100000000000000000001f4c8f927aed3ca752257
    }
  , affine = Affine
    { xA = PF 0x4a96b5688ef573284664698968c38bb913cbfc82
    , yA = PF 0x23a628553168947d59dcc912042351377ac5fb32
    }
  }

secp160r2 :: Curve
secp160r2 = Curve
  { types = Types
    { curve   = "SECP160R2"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffeffffac73
    , field'  = PrimeField "Fr" 0x100000000000000000000351ee786a818f3a1a16b
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xfffffffffffffffffffffffffffffffeffffac70
    , b = PF 0xb4e134d3fb59eb8bab57274904664d5af50388ba
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffeffffac73
    , r = 0x100000000000000000000351ee786a818f3a1a16b
    }
  , affine = Affine
    { xA = PF 0x52dcb034293a117e1f4ff11b30f7199d3144ce6d
    , yA = PF 0xfeaffef2e331f296e071fa0df9982cfea7d43f2e
    }
  }

secp192k1 :: Curve
secp192k1 = Curve
  { types = Types
    { curve   = "SECP192K1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffeffffee37
    , field'  = PrimeField "Fr" 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x3
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffffffffffeffffee37
    , r = 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
    }
  , affine = Affine
    { xA = PF 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
    , yA = PF 0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
    }
  }

secp192r1 :: Curve
secp192r1 = Curve
  { types = Types
    { curve   = "SECP192R1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffeffffffffffffffff
    , field'  = PrimeField "Fr" 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xfffffffffffffffffffffffffffffffefffffffffffffffc
    , b = PF 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
    , r = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
    }
  , affine = Affine
    { xA = PF 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
    , yA = PF 0x7192b95ffc8da78631011ed6b24cdd573f977a11e794811
    }
  }

secp224k1 :: Curve
secp224k1 = Curve
  { types = Types
    { curve   = "SECP224K1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
    , field'  = PrimeField "Fr" 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x5
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
    , r = 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
    }
  , affine = Affine
    { xA = PF 0xa1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
    , yA = PF 0x7e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
    }
  }

secp224r1 :: Curve
secp224r1 = Curve
  { types = Types
    { curve   = "SECP224R1"
    , field   = PrimeField "Fq" 0xffffffffffffffffffffffffffffffff000000000000000000000001
    , field'  = PrimeField "Fr" 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
    , b = PF 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
    , h = 0x1
    , q = 0xffffffffffffffffffffffffffffffff000000000000000000000001
    , r = 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
    }
  , affine = Affine
    { xA = PF 0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
    , yA = PF 0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
    }
  }

secp256k1 :: Curve
secp256k1 = Curve
  { types = Types
    { curve   = "SECP256K1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
    , field'  = PrimeField "Fr" 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x0
    , b = PF 0x7
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
    , r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    }
  , affine = Affine
    { xA = PF 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
    , yA = PF 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
    }
  }

secp256r1 :: Curve
secp256r1 = Curve
  { types = Types
    { curve   = "SECP256R1"
    , field   = PrimeField "Fq" 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
    , b = PF 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
    , h = 0x1
    , q = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
    , r = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
    }
  , affine = Affine
    { xA = PF 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
    , yA = PF 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
    }
  }

secp384r1 :: Curve
secp384r1 = Curve
  { types = Types
    { curve   = "SECP384R1"
    , field   = PrimeField "Fq" 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
    , field'  = PrimeField "Fr" 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc
    , b = PF 0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
    , h = 0x1
    , q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
    , r = 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
    }
  , affine = Affine
    { xA = PF 0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7
    , yA = PF 0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f
    }
  }

secp521r1 :: Curve
secp521r1 = Curve
  { types = Types
    { curve   = "SECP521R1"
    , field   = PrimeField "Fq" 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , field'  = PrimeField "Fr" 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
    , imports = Nothing
    }
  , parameters = Parameters
    { a = PF 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc
    , b = PF 0x51953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
    , h = 0x1
    , q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , r = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
    }
  , affine = Affine
    { xA = PF 0xc6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66
    , yA = PF 0x11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650
    }
  }
