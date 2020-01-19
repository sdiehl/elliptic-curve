module DiffieHellman where

import Data.Curve.Montgomery.Curve25519 as Curve25519
import qualified Data.Field.Galois as Fr
import Protolude

-- Generate random private point on Curve25519
generate :: IO Curve25519.Fr
generate = Fr.rnd

main :: IO ()
main = do
  alice_private <- generate
  bob_private <- generate
  let bob_public :: Curve25519.PA
      bob_public = gen `mul` bob_private
      alice_public :: Curve25519.PA
      alice_public = gen `mul` alice_private
  let shared_secret1 = bob_public `mul` alice_private
      shared_secret2 = alice_public `mul` bob_private
  putText "Alice Private Key:"
  print alice_private
  putText "Alice Public Key:"
  print alice_public
  putText "Bob Private Key:"
  print bob_private
  putText "Bob Public Key:"
  print bob_public
  putText "Diffie Hellman Shared Secret"
  print shared_secret1
  print (shared_secret1 == shared_secret2)
  pure ()
