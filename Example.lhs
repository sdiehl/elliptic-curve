import Data.Curve.Edwards.Ed25519 as Ed25519
import Protolude

-- generate random affine ponit
p1 :: Ed25519.PA
p1 = Ed25519.gen

-- generate affine point by multiply by scalar
p2 :: Ed25519.PA
p2 = Ed25519.mul p1 (3 :: Ed25519.Fr)

-- ** --

-- point addition
p3 :: Ed25519.PA
p3 = Ed25519.add p1 p2

-- point identity
p4 :: Ed25519.PA
p4 = Ed25519.id

-- point doubling
p5 :: Ed25519.PA
p5 = Ed25519.dbl p1

-- point inversion
p6 :: Ed25519.PA
p6 = Ed25519.inv p1

-- Frobenius endomorphism
p7 :: Ed25519.PA
p7 = Ed25519.frob p1

-- base point
p8 :: Ed25519.PA
p8 = Ed25519.gA

-- convert affine to projective
p9 :: Ed25519.PP
p9 = fromA p8

-- get y coordinate from coordinate
p10 :: Maybe Ed25519.Fq
p10 = yX p8 (2 :: Fq)
