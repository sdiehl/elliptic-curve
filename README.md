<p align="center">
  <a href="http://www.adjoint.io">
    <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
  </a>
</p>

# Elliptic Curve

An extensible library of elliptic curves used in cryptography research.

## Curve representations

An [**elliptic curve**](src/Data/Curve.hs) E(K) over a field K is a *smooth projective plane algebraic cubic curve* with a specified base point `O`, and the *points* on E(K) form an *algebraic group* with identity point `O`. By the *Riemann-Roch theorem*, any elliptic curve is isomorphic to a cubic curve of the form

<p align="center"><img src="/tex/7c1ba96d0dc75a92a1e33ca2fcfde4ac.svg?invert_in_darkmode&sanitize=true" align=middle width=458.10354314999995pt height=18.312383099999998pt/></p>

where <img src="/tex/9afe6a256a9817c76b579e6f5db9a578.svg?invert_in_darkmode&sanitize=true" align=middle width=12.99542474999999pt height=22.465723500000017pt/> is the *point at infinity*, and <img src="/tex/cec3de417413798b6c3d739decc1abce.svg?invert_in_darkmode&sanitize=true" align=middle width=108.71968964999999pt height=14.15524440000002pt/> are *K-rational coefficients* that satisfy a *non-zero discriminant* condition. For cryptographic computational purposes, elliptic curves are represented in several different forms.

### Weierstrass curves

A (short) [**Weierstrass curve**](src/Data/Curve/Weierstrass.hs) is an elliptic curve over <img src="/tex/cdeab355fe7188f5cb733d1a3e5b73d1.svg?invert_in_darkmode&sanitize=true" align=middle width=44.686187699999984pt height=24.65753399999998pt/> for some prime <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/>, and is of the form

<p align="center"><img src="/tex/7f4099e4c9e1b8bc7227bb1c3947e3f3.svg?invert_in_darkmode&sanitize=true" align=middle width=338.74327905pt height=18.312383099999998pt/></p>

where `A` and `B` are K-rational coefficients such that <img src="/tex/e5f279727fb95b805306e70125f30018.svg?invert_in_darkmode&sanitize=true" align=middle width=84.29802809999998pt height=26.76175259999998pt/> is non-zero. Weierstrass curves are the most common representations of elliptic curves, as any elliptic curve over a field of characteristic greater than 3 is isomorphic to a Weierstrass curve.

### Binary curves

A (short Weierstrass) [**binary curve**](src/Data/Curve/Binary.hs) is an elliptic curve over <img src="/tex/f4687471921caacf38fd0e0667005c1f.svg?invert_in_darkmode&sanitize=true" align=middle width=57.12159419999999pt height=24.65753399999998pt/> for some positive <img src="/tex/0e51a2dede42189d77627c4d742822c3.svg?invert_in_darkmode&sanitize=true" align=middle width=14.433101099999991pt height=14.15524440000002pt/>, and is of the form

<p align="center"><img src="/tex/b1771b4a57a60a91d6f0ee77fdd140d0.svg?invert_in_darkmode&sanitize=true" align=middle width=341.6558475pt height=18.312383099999998pt/></p>

where `A` and `B` are K-rational coefficients such that `B` is non-zero. Binary curves have field elements represented by binary integers for efficient arithmetic, and are special cases of *long Weierstrass curves* over a field of characteristic 2.

### Montgomery curves

A [**Montgomery curve**](src/Data/Curve/Montgomery.hs) is an elliptic curve over <img src="/tex/cdeab355fe7188f5cb733d1a3e5b73d1.svg?invert_in_darkmode&sanitize=true" align=middle width=44.686187699999984pt height=24.65753399999998pt/> for some prime <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/>, and is of the form

<p align="center"><img src="/tex/411d1a91b2998b19b1797a8c48655a1f.svg?invert_in_darkmode&sanitize=true" align=middle width=329.55131715pt height=18.312383099999998pt/></p>

where `A` and `B` are K-rational coefficients such that <img src="/tex/dcfbb15abd46e172d6ade9ec5402a60f.svg?invert_in_darkmode&sanitize=true" align=middle width=74.09249594999999pt height=26.76175259999998pt/> is non-zero. Montgomery curves only use the first affine coordinate for computations, and can utilise the Montgomery ladder for efficient multiplication.

### Edwards curves

A (twisted) [**Edwards curve**](src/Data/Curve/Edwards.hs) is an elliptic curve over <img src="/tex/cdeab355fe7188f5cb733d1a3e5b73d1.svg?invert_in_darkmode&sanitize=true" align=middle width=44.686187699999984pt height=24.65753399999998pt/> for some prime <img src="/tex/2ec6e630f199f589a2402fdf3e0289d5.svg?invert_in_darkmode&sanitize=true" align=middle width=8.270567249999992pt height=14.15524440000002pt/>, and is of the form

<p align="center"><img src="/tex/9f0965c23101451fdcef241119b2c1ca.svg?invert_in_darkmode&sanitize=true" align=middle width=296.65172954999997pt height=18.312383099999998pt/></p>

where `A` and `D` are K-rational coefficients such that <img src="/tex/a99b826773674a6f62e90ce098504e47.svg?invert_in_darkmode&sanitize=true" align=middle width=69.22829594999999pt height=24.65753399999998pt/> is non-zero. Edwards curves have no point at infinity, and their addition and doubling formulae converge.

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

```haskell ignore
data Anomalous

type Fq = Prime Q
type Q = 0xb0000000000000000000000953000000000000000000001f9d7

type Fr = Prime R
type R = 0xb0000000000000000000000953000000000000000000001f9d7

instance Curve 'Weierstrass c Anomalous Fq Fr => WCurve c Anomalous Fq Fr where
-- data instance Point 'Weierstrass c Anomalous Fq Fr
```

**Arithmetic**

```haskell ignore
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

```haskell ignore
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

See [**Example.hs**](Example.hs).

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
