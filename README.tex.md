<p align="center">
  <a href="http://www.adjoint.io">
    <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
  </a>
</p>

# Elliptic Curve

An extensible library of elliptic curves used in cryptography research.

## Curve representations

An [**elliptic curve**](src/Data/Curve.hs) E(K) over a field K is a *smooth projective plane algebraic cubic curve* with a specified base point `O`, and the *points* on E(K) form an *algebraic group* with identity point `O`. By the *Riemann-Roch theorem*, any elliptic curve is isomorphic to a cubic curve of the form

$$
E(K) = \{ (x, y) | y^2 + a_1 xy + a_3 y = x^3 + a_2 x^2 + a_4 x + a_6 \} \cup \{O\}
$$

where $O$ is the *point at infinity*, and $a_1, a_2, a_3, a_4, a_6$ are *K-rational coefficients* that satisfy a *non-zero discriminant* condition. For cryptographic computational purposes, elliptic curves are represented in several different forms.

### Weierstrass curves

A (short) [**Weierstrass curve**](src/Data/Curve/Weierstrass.hs) is an elliptic curve over $\text{GF}(p)$ for some prime $p$, and is of the form

$$
E(GF(p)) = \{ (x, y) | y^2 = x^3 + Ax^2 + B \} \cup \{O\}
$$

where `A` and `B` are K-rational coefficients such that $4A^3 + 27B^2$ is non-zero. Weierstrass curves are the most common representations of elliptic curves, as any elliptic curve over a field of characteristic greater than 3 is isomorphic to a Weierstrass curve.

### Binary curves

A (short Weierstrass) [**binary curve**](src/Data/Curve/Binary.hs) is an elliptic curve over $\text{GF}(2^m)$ for some positive $m$, and is of the form

$$
E(\text{GF}(2^m)) = \{ (x, y) | y^2 = x^3 + Ax + B \} \cup \{O\}
$$

where `A` and `B` are K-rational coefficients such that `B` is non-zero. Binary curves have field elements represented by binary integers for efficient arithmetic, and are special cases of *long Weierstrass curves* over a field of characteristic 2.

### Montgomery curves

A [**Montgomery curve**](src/Data/Curve/Montgomery.hs) is an elliptic curve over $\text{GF}(p)$ for some prime $p$, and is of the form

$$
E(\text{GF}(p)) = {(x, y) | By^2 = x^3 + Ax^2 + x} \cup \{O\}
$$

where `A` and `B` are K-rational coefficients such that $B(A^2 - 4)$ is non-zero. Montgomery curves only use the first affine coordinate for computations, and can utilise the Montgomery ladder for efficient multiplication.

### Edwards curves

A (twisted) [**Edwards curve**](src/Data/Curve/Edwards.hs) is an elliptic curve over $\text{GF}(p)$ for some prime $p$, and is of the form

$$
E(\text{GF}(p)) = {(x, y) | Ax^2 + y^2 = 1 + Dx^2y^2}
$$

where `A` and `D` are K-rational coefficients such that $D(1 - D)$ is non-zero. Edwards curves have no point at infinity, and their addition and doubling formulae converge.

## Curve usage

This library is open for new curve representations and curve implementations through pull requests. These should ideally be executed by replicating and modifying existing curve files, for ease, quickcheck testing, and formatting consistency, but a short description of the file organisation is provided here for clarity. Note that it also has a dependency on the [Galois field library](https://github.com/adjoint-io/galois-field) and its required language extensions.

The library exposes four promoted data kinds which are used to define a
type-safe interface for working with curves.

**Forms**

* Binary
* Edwards
* Montgomery
* Weierstrass

**Coordinates**

* Affine
* Jacobian
* Projective

These are then specialised down into type classes for the different forms.

* BCurve
* ECurve
* MCurve
* WCurve

And then by coordinate system.

* BACurve
* BPCurve
* EACurve
* EPCurve
* MACurve
* WACurve
* WJCurve
* WPCurve

A curve class is constructed out of four type paramaters which are instantiated
in the associated data type Point on the Curve typeclass.

```text
class Curve (f :: Form) (c :: Coordinates) e q r
                                           | | |
                              Curve Type o-+ | |
                         Field of Points o---+ |
                   Field of Coefficients o-----+

data Point f c e q r :: *
```

For example:

```haskell
data Anomalous

type Fq = Prime Q
type Q = 0xb0000000000000000000000953000000000000000000001f9d7

type Fr = Prime R
type R = 0xb0000000000000000000000953000000000000000000001f9d7

instance Curve 'Weierstrass c Anomalous Fq Fr => WCurve c Anomalous Fq Fr where
-- data instance Point 'Weierstrass c Anomalous Fq Fr
```

**Arithmetic**

```haskell
-- Point addition
add :: Point f c e q r -> Point f c e q r -> Point f c e q r

-- Point doubling
dbl :: Point f c e q r -> Point f c e q r

-- Point multiplication by field element
mul :: Curve f c e q r => Point f c e q r -> r -> Point f c e q r

-- Point multiplication by Integral
mul' :: (Curve f c e q r, Integral n) => Point f c e q r -> n -> Point f c e q r

-- Point identity
id :: Point f c e q r

-- Point inversion
inv :: Point f c e q r -> Point f c e q r

-- Frobenius endomorphism
frob :: Point f c e q r -> Point f c e q r

-- Random point
rnd :: MonadRandom m => m (Point f c e q r)
```

**Other Functions**

```haskell
-- Curve characteristic 
char :: Point f c e q r -> Natural

-- Curve cofactor
cof :: Point f c e q r -> Natural

-- Check if a point is well-defined
def :: Point f c e q r -> Bool

-- Discriminant
disc :: Point f c e q r -> q

-- Curve order
order :: Point f c e q r -> Natural

-- Curve generator point
gen :: Point f c e q r
```

### Representing a new curve using the curve class

E.g. See [**Weierstrass**](src/Data/Curve/Weierstrass.hs).

### Implementing a new curve using a curve representation

E.g. See [**Anomalous**](src/Data/Curve/Weierstrass/Anomalous.hs).

### Using an implemented curve

Import a curve implementation.
```haskell
import qualified Data.Curve.Weierstrass.Anomalous as Anomalous
```
The data types and constants can then be accessed readily as `Anomalous.PA` and `Anomalous._g`.

We'll test that the Hasse Theorem is successful with an implemented curve as a usage example:
```haskell
import Protolude
import GHC.Natural
import qualified Data.Field.Galois as F

main :: IO ()
main = do
    putText $ "Hasse Theorem succeeds: " <> show (hasseTheorem Anomalous._h Anomalous._r (F.order (witness :: Anomalous.Fq)))
  where
    hasseTheorem h r q = join (*) (naturalToInteger (h * r) - naturalToInteger q - 1) <= 4 * naturalToInteger q
```

### Point Arithmetic

```haskell
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
```

## Curve implementations

The following curves have already been implemented.

### Binary curves

* SECT (NIST) curves
  * [SECT113R1](src/Data/Curve/Binary/SECT113R1.hs)
  * [SECT113R2](src/Data/Curve/Binary/SECT113R2.hs)
  * [SECT131R1](src/Data/Curve/Binary/SECT131R1.hs)
  * [SECT131R2](src/Data/Curve/Binary/SECT131R2.hs)
  * [SECT163K1](src/Data/Curve/Binary/SECT163K1.hs)
  * [SECT163R1](src/Data/Curve/Binary/SECT163R1.hs)
  * [SECT163R2](src/Data/Curve/Binary/SECT163R2.hs)
  * [SECT193R1](src/Data/Curve/Binary/SECT193R1.hs)
  * [SECT193R2](src/Data/Curve/Binary/SECT193R2.hs)
  * [SECT233K1](src/Data/Curve/Binary/SECT233K1.hs)
  * [SECT233R1](src/Data/Curve/Binary/SECT233R1.hs)
  * [SECT239K1](src/Data/Curve/Binary/SECT239K1.hs)
  * [SECT283K1](src/Data/Curve/Binary/SECT283K1.hs)
  * [SECT283R1](src/Data/Curve/Binary/SECT283R1.hs)
  * [SECT409K1](src/Data/Curve/Binary/SECT409K1.hs)
  * [SECT409R1](src/Data/Curve/Binary/SECT409R1.hs)
  * [SECT571K1](src/Data/Curve/Binary/SECT571K1.hs)
  * [SECT571R1](src/Data/Curve/Binary/SECT571R1.hs)

### Edwards curves

* Edwards curves
  * [Curve1174](src/Data/Curve/Edwards/Curve1174.hs)
  * [Curve41417](src/Data/Curve/Edwards/Curve41417.hs)
  * [E-222](src/Data/Curve/Edwards/E222.hs)
  * [E-382](src/Data/Curve/Edwards/E382.hs)
  * [E-521](src/Data/Curve/Edwards/E521.hs)
  * [Ed25519 (Curve25519)](src/Data/Curve/Edwards/Ed25519.hs)
  * [Ed3363 (HighFive)](src/Data/Curve/Edwards/Ed3363.hs)
  * [Ed448 (Goldilocks)](src/Data/Curve/Edwards/Ed448.hs)
  * [JubJub](src/Data/Curve/Edwards/JubJub.hs)

### Montgomery curves

* Montgomery curves
  * [Curve25519 (Ed25519)](src/Data/Curve/Montgomery/Curve25519.hs)
  * [Curve383187](src/Data/Curve/Montgomery/Curve383187.hs)
  * [Curve448 (Goldilocks)](src/Data/Curve/Montgomery/Curve448.hs)
  * [M221](src/Data/Curve/Montgomery/M221.hs)
  * [M383](src/Data/Curve/Montgomery/M383.hs)
  * [M511](src/Data/Curve/Montgomery/M511.hs)

### Weierstrass curves

* [Anomalous](src/Data/Curve/Weierstrass/Anomalous.hs)
* [ANSSIFRP256V1](src/Data/Curve/Weierstrass/ANSSIFRP256V1.hs)
* Barreto-Lynn-Scott (BLS) curves
  * [BLS12381](src/Data/Curve/Weierstrass/BLS12381.hs)
  * [BLS12381T](src/Data/Curve/Weierstrass/BLS12381T.hs)
  * [BLS48581](src/Data/Curve/Weierstrass/BLS48581.hs)
  * [BLS48581T](src/Data/Curve/Weierstrass/BLS48581T.hs)
* Barreto-Naehrig (BN) curves
  * [BN224 (Fp224BN)](src/Data/Curve/Weierstrass/BN224.hs)
  * [BN254 (CurveSNARK)](src/Data/Curve/Weierstrass/BN254.hs)
  * [BN254T (CurveSNARKn2)](src/Data/Curve/Weierstrass/BN254T.hs)
  * [BN254A (Fp254BNa)](src/Data/Curve/Weierstrass/BN254A.hs)
  * [BN254AT (Fp254n2BNa)](src/Data/Curve/Weierstrass/BN254AT.hs)
  * [BN254B (Fp254BNb)](src/Data/Curve/Weierstrass/BN254B.hs)
  * [BN254BT (Fp254n2BNb)](src/Data/Curve/Weierstrass/BN254BT.hs)
  * [BN254C (Fp254BNc)](src/Data/Curve/Weierstrass/BN254C.hs)
  * [BN254CT (Fp254n2BNc)](src/Data/Curve/Weierstrass/BN254CT.hs)
  * [BN254D (Fp254BNd)](src/Data/Curve/Weierstrass/BN254D.hs)
  * [BN254DT (Fp254n2BNd)](src/Data/Curve/Weierstrass/BN254DT.hs)
  * [BN256 (Fp256BN)](src/Data/Curve/Weierstrass/BN256.hs)
  * [BN384 (Fp384BN)](src/Data/Curve/Weierstrass/BN384.hs)
  * [BN462 (Fp462BN)](src/Data/Curve/Weierstrass/BN462.hs)
  * [BN462T (Fp462n2BN)](src/Data/Curve/Weierstrass/BN462T.hs)
  * [BN512 (Fp512BN)](src/Data/Curve/Weierstrass/BN512.hs)
* Brainpool curves
  * [BrainpoolP160R1](src/Data/Curve/Weierstrass/BrainpoolP160R1.hs)
  * [BrainpoolP160T1](src/Data/Curve/Weierstrass/BrainpoolP160T1.hs)
  * [BrainpoolP192R1](src/Data/Curve/Weierstrass/BrainpoolP192R1.hs)
  * [BrainpoolP192T1](src/Data/Curve/Weierstrass/BrainpoolP192T1.hs)
  * [BrainpoolP224R1](src/Data/Curve/Weierstrass/BrainpoolP224R1.hs)
  * [BrainpoolP224T1](src/Data/Curve/Weierstrass/BrainpoolP224T1.hs)
  * [BrainpoolP256R1](src/Data/Curve/Weierstrass/BrainpoolP256R1.hs)
  * [BrainpoolP256T1](src/Data/Curve/Weierstrass/BrainpoolP256T1.hs)
  * [BrainpoolP320R1](src/Data/Curve/Weierstrass/BrainpoolP320R1.hs)
  * [BrainpoolP320T1](src/Data/Curve/Weierstrass/BrainpoolP320T1.hs)
  * [BrainpoolP384R1](src/Data/Curve/Weierstrass/BrainpoolP384R1.hs)
  * [BrainpoolP384T1](src/Data/Curve/Weierstrass/BrainpoolP384T1.hs)
  * [BrainpoolP512R1](src/Data/Curve/Weierstrass/BrainpoolP512R1.hs)
  * [BrainpoolP512T1](src/Data/Curve/Weierstrass/BrainpoolP512T1.hs)
* SECP (NIST) curves
  * [SECP112R1](src/Data/Curve/Weierstrass/SECP112R1.hs)
  * [SECP112R2](src/Data/Curve/Weierstrass/SECP112R2.hs)
  * [SECP128R1](src/Data/Curve/Weierstrass/SECP128R1.hs)
  * [SECP128R2](src/Data/Curve/Weierstrass/SECP128R2.hs)
  * [SECP160K1](src/Data/Curve/Weierstrass/SECP160K1.hs)
  * [SECP160R1](src/Data/Curve/Weierstrass/SECP160R1.hs)
  * [SECP160R2](src/Data/Curve/Weierstrass/SECP160R2.hs)
  * [SECP192K1](src/Data/Curve/Weierstrass/SECP192K1.hs)
  * [SECP192R1](src/Data/Curve/Weierstrass/SECP192R1.hs)
  * [SECP224K1](src/Data/Curve/Weierstrass/SECP224K1.hs)
  * [SECP224R1](src/Data/Curve/Weierstrass/SECP224R1.hs)
  * [SECP256K1](src/Data/Curve/Weierstrass/SECP256K1.hs)
  * [SECP256R1](src/Data/Curve/Weierstrass/SECP256R1.hs)
  * [SECP384R1](src/Data/Curve/Weierstrass/SECP384R1.hs)
  * [SECP521R1](src/Data/Curve/Weierstrass/SECP521R1.hs)

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.
