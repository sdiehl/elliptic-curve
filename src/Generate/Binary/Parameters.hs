module Generate.Binary.Parameters
  ( curves
  ) where

import Protolude

import Generate.Binary.Types

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

curves :: [Curve]
curves =
  [ sect113r1
  , sect113r2
  , sect131r1
  , sect131r2
  , sect163k1
  , sect163r1
  , sect163r2
  , sect193r1
  , sect193r2
  , sect233k1
  , sect233r1
  , sect239k1
  , sect283k1
  , sect283r1
  , sect409k1
  , sect409r1
  , sect571k1
  , sect571r1
  ]

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

sect113r1 :: Curve
sect113r1 = Curve
  { types = Types
    { curve   = "SECT113R1"
    , field   = BinaryField "F2m" 0x20000000000000000000000000201
    , field'  = PrimeField "Fr" 0x100000000000000d9ccec8a39e56f
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x3088250ca6e7c7fe649ce85820f7
    , b = BF 0xe8bee4d3e2260744188be0e9c723
    , h = 0x2
    , p = 0x20000000000000000000000000201
    , r = 0x100000000000000d9ccec8a39e56f
    , x = BF 0x9d73616f35f4ab1407d73562c10f
    , y = BF 0xa52830277958ee84d1315ed31886
    }
  }

sect113r2 :: Curve
sect113r2 = Curve
  { types = Types
    { curve   = "SECT113R2"
    , field   = BinaryField "F2m" 0x20000000000000000000000000201
    , field'  = PrimeField "Fr" 0x10000000000000108789b2496af93
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x689918dbec7e5a0dd6dfc0aa55c7
    , b = BF 0x95e9a9ec9b297bd4bf36e059184f
    , h = 0x2
    , p = 0x20000000000000000000000000201
    , r = 0x10000000000000108789b2496af93
    , x = BF 0x1a57a6a7b26ca5ef52fcdb8164797
    , y = BF 0xb3adc94ed1fe674c06e695baba1d
    }
  }

sect131r1 :: Curve
sect131r1 = Curve
  { types = Types
    { curve   = "SECT131R1"
    , field   = BinaryField "F2m" 0x80000000000000000000000000000010d
    , field'  = PrimeField "Fr" 0x400000000000000023123953a9464b54d
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x7a11b09a76b562144418ff3ff8c2570b8
    , b = BF 0x217c05610884b63b9c6c7291678f9d341
    , h = 0x2
    , p = 0x80000000000000000000000000000010d
    , r = 0x400000000000000023123953a9464b54d
    , x = BF 0x81baf91fdf9833c40f9c181343638399
    , y = BF 0x78c6e7ea38c001f73c8134b1b4ef9e150
    }
  }

sect131r2 :: Curve
sect131r2 = Curve
  { types = Types
    { curve   = "SECT131R2"
    , field   = BinaryField "F2m" 0x80000000000000000000000000000010d
    , field'  = PrimeField "Fr" 0x400000000000000016954a233049ba98f
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x3e5a88919d7cafcbf415f07c2176573b2
    , b = BF 0x4b8266a46c55657ac734ce38f018f2192
    , h = 0x2
    , p = 0x80000000000000000000000000000010d
    , r = 0x400000000000000016954a233049ba98f
    , x = BF 0x356dcd8f2f95031ad652d23951bb366a8
    , y = BF 0x648f06d867940a5366d9e265de9eb240f
    }
  }

sect163k1 :: Curve
sect163k1 = Curve
  { types = Types
    { curve   = "SECT163K1"
    , field   = BinaryField "F2m" 0x800000000000000000000000000000000000000c9
    , field'  = PrimeField "Fr" 0x4000000000000000000020108a2e0cc0d99f8a5ef
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x1
    , h = 0x2
    , p = 0x800000000000000000000000000000000000000c9
    , r = 0x4000000000000000000020108a2e0cc0d99f8a5ef
    , x = BF 0x2fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
    , y = BF 0x289070fb05d38ff58321f2e800536d538ccdaa3d9
    }
  }

sect163r1 :: Curve
sect163r1 = Curve
  { types = Types
    { curve   = "SECT163R1"
    , field   = BinaryField "F2m" 0x800000000000000000000000000000000000000c9
    , field'  = PrimeField "Fr" 0x3ffffffffffffffffffff48aab689c29ca710279b
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x7b6882caaefa84f9554ff8428bd88e246d2782ae2
    , b = BF 0x713612dcddcb40aab946bda29ca91f73af958afd9
    , h = 0x2
    , p = 0x800000000000000000000000000000000000000c9
    , r = 0x3ffffffffffffffffffff48aab689c29ca710279b
    , x = BF 0x369979697ab43897789566789567f787a7876a654
    , y = BF 0x435edb42efafb2989d51fefce3c80988f41ff883
    }
  }

sect163r2 :: Curve
sect163r2 = Curve
  { types = Types
    { curve   = "SECT163R2"
    , field   = BinaryField "F2m" 0x800000000000000000000000000000000000000c9
    , field'  = PrimeField "Fr" 0x40000000000000000000292fe77e70c12a4234c33
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x20a601907b8c953ca1481eb10512f78744a3205fd
    , h = 0x2
    , p = 0x800000000000000000000000000000000000000c9
    , r = 0x40000000000000000000292fe77e70c12a4234c33
    , x = BF 0x3f0eba16286a2d57ea0991168d4994637e8343e36
    , y = BF 0xd51fbc6c71a0094fa2cdd545b11c5c0c797324f1
    }
  }

sect193r1 :: Curve
sect193r1 = Curve
  { types = Types
    { curve   = "SECT193R1"
    , field   = BinaryField "F2m" 0x2000000000000000000000000000000000000000000008001
    , field'  = PrimeField "Fr" 0x1000000000000000000000000c7f34a778f443acc920eba49
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x17858feb7a98975169e171f77b4087de098ac8a911df7b01
    , b = BF 0xfdfb49bfe6c3a89facadaa7a1e5bbc7cc1c2e5d831478814
    , h = 0x2
    , p = 0x2000000000000000000000000000000000000000000008001
    , r = 0x1000000000000000000000000c7f34a778f443acc920eba49
    , x = BF 0x1f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
    , y = BF 0x25e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
    }
  }

sect193r2 :: Curve
sect193r2 = Curve
  { types = Types
    { curve   = "SECT193R2"
    , field   = BinaryField "F2m" 0x2000000000000000000000000000000000000000000008001
    , field'  = PrimeField "Fr" 0x10000000000000000000000015aab561b005413ccd4ee99d5
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
    , b = BF 0xc9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
    , h = 0x2
    , p = 0x2000000000000000000000000000000000000000000008001
    , r = 0x10000000000000000000000015aab561b005413ccd4ee99d5
    , x = BF 0xd9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
    , y = BF 0x1ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
    }
  }

sect233k1 :: Curve
sect233k1 = Curve
  { types = Types
    { curve   = "SECT233K1"
    , field   = BinaryField "F2m" 0x20000000000000000000000000000000000000004000000000000000001
    , field'  = PrimeField "Fr" 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x0
    , b = BF 0x1
    , h = 0x4
    , p = 0x20000000000000000000000000000000000000004000000000000000001
    , r = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
    , x = BF 0x17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
    , y = BF 0x1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
    }
  }

sect233r1 :: Curve
sect233r1 = Curve
  { types = Types
    { curve   = "SECT233R1"
    , field   = BinaryField "F2m" 0x20000000000000000000000000000000000000004000000000000000001
    , field'  = PrimeField "Fr" 0x1000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x66647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
    , h = 0x2
    , p = 0x20000000000000000000000000000000000000004000000000000000001
    , r = 0x1000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
    , x = BF 0xfac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
    , y = BF 0x1006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
    }
  }

sect239k1 :: Curve
sect239k1 = Curve
  { types = Types
    { curve   = "SECT239K1"
    , field   = BinaryField "F2m" 0x800000000000000000004000000000000000000000000000000000000001
    , field'  = PrimeField "Fr" 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x0
    , b = BF 0x1
    , h = 0x4
    , p = 0x800000000000000000004000000000000000000000000000000000000001
    , r = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
    , x = BF 0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
    , y = BF 0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
    }
  }

sect283k1 :: Curve
sect283k1 = Curve
  { types = Types
    { curve   = "SECT283K1"
    , field   = BinaryField "F2m" 0x800000000000000000000000000000000000000000000000000000000000000000010a1
    , field'  = PrimeField "Fr" 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x0
    , b = BF 0x1
    , h = 0x4
    , p = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
    , r = 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
    , x = BF 0x503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
    , y = BF 0x1ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
    }
  }

sect283r1 :: Curve
sect283r1 = Curve
  { types = Types
    { curve   = "SECT283R1"
    , field   = BinaryField "F2m" 0x800000000000000000000000000000000000000000000000000000000000000000010a1
    , field'  = PrimeField "Fr" 0x3ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x27b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
    , h = 0x2
    , p = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
    , r = 0x3ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
    , x = BF 0x5f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
    , y = BF 0x3676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
    }
  }

sect409k1 :: Curve
sect409k1 = Curve
  { types = Types
    { curve   = "SECT409K1"
    , field   = BinaryField "F2m" 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
    , field'  = PrimeField "Fr" 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x0
    , b = BF 0x1
    , h = 0x4
    , p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
    , r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
    , x = BF 0x60f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746
    , y = BF 0x1e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b
    }
  }

sect409r1 :: Curve
sect409r1 = Curve
  { types = Types
    { curve   = "SECT409R1"
    , field   = BinaryField "F2m" 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
    , field'  = PrimeField "Fr" 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x21a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
    , h = 0x2
    , p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
    , r = 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
    , x = BF 0x15d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
    , y = BF 0x61b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
    }
  }

sect571k1 :: Curve
sect571k1 = Curve
  { types = Types
    { curve   = "SECT571K1"
    , field   = BinaryField "F2m" 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
    , field'  = PrimeField "Fr" 0x20000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x0
    , b = BF 0x1
    , h = 0x4
    , p = 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
    , r = 0x20000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
    , x = BF 0x26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
    , y = BF 0x349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
    }
  }

sect571r1 :: Curve
sect571r1 = Curve
  { types = Types
    { curve   = "SECT571R1"
    , field   = BinaryField "F2m" 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
    , field'  = PrimeField "Fr" 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47
    , imports = Nothing
    }
  , parameters = Parameters
    { a = BF 0x1
    , b = BF 0x2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a
    , h = 0x2
    , p = 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
    , r = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47
    , x = BF 0x303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19
    , y = BF 0x37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b
    }
  }
