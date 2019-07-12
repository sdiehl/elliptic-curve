<p align="center">
  <a href="http://www.adjoint.io">
    <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
  </a>
</p>

# Elliptic Curve

An extensible library of elliptic curves used in cryptography research.

## Curve representations

An [**elliptic curve**](src/Curve.hs) E(K) over a field K is a *smooth projective plane algebraic cubic curve* with a specified base point `O`, and the *points* on E(K) form an *algebraic group* with identity point `O`. By the *Riemann-Roch theorem*, any elliptic curve is isomorphic to a cubic curve of the form
```
E(K) = {(x, y) | y^2 + a1xy + a3y = x^3 + a2x^2 + a4x + a6} U {O}
```
where `O` is the *point at infinity*, and `a1, a2, a3, a4, a6` are *K-rational coefficients* that satisfy a *non-zero discriminant* condition. For cryptographic computational purposes, elliptic curves are represented in several different forms.

### Weierstrass curves

A (short) [**Weierstrass curve**](src/Curve/Weierstrass.hs) is an elliptic curve over GF(p) for some prime p, and is of the form
```
E(GF(p)) = {(x, y) | y^2 = x^3 + Ax^2 + B} U {O}
```
where `A` and `B` are K-rational coefficients such that `4A^3 + 27B^2` is non-zero. Weierstrass curves are the most common representations of elliptic curves, as any elliptic curve over a field of characteristic greater than 3 is isomorphic to a Weierstrass curve.

### Binary curves

A (short Weierstrass) [**binary curve**](src/Curve/Binary.hs) is an elliptic curve over GF(2^m) for some positive m, and is of the form
```
E(GF(2^m)) = {(x, y) | y^2 = x^3 + Ax + B} U {O}
```
where `A` and `B` are K-rational coefficients such that `B` is non-zero. Binary curves have field elements represented by binary integers for efficient arithmetic, and are special cases of *long Weierstrass curves* over a field of characteristic 2.

### Montgomery curves

A [**Montgomery curve**](src/Curve/Montgomery.hs) is an elliptic curve over GF(p) for some prime p, and is of the form
```
E(GF(p)) = {(x, y) | By^2 = x^3 + Ax^2 + x} U {O}
```
where `A` and `B` are K-rational coefficients such that `B(A^2 - 4)` is non-zero. Montgomery curves only use the first affine coordinate for computations, and can utilise the Montgomery ladder for efficient multiplication.

### Edwards curves

A (twisted) [**Edwards curve**](src/Curve/Edwards.hs) is an elliptic curve over GF(p) for some prime p, and is of the form
```
E(GF(p)) = {(x, y) | Ax^2 + y^2 = 1 + Dx^2y^2}
```
where `A` and `D` are K-rational coefficients such that `D(1 - D)` is non-zero. Edwards curves have no point at infinity, and their addition and doubling formulae converge.

## Curve usage

TODO

## Curve implementations

The following curves have already been implemented.

### Binary curves

* SECT (NIST) curves
  * [SECT113R1](src/Curve/Binary/SECT113R1.hs)
  * [SECT113R2](src/Curve/Binary/SECT113R2.hs)
  * [SECT131R1](src/Curve/Binary/SECT131R1.hs)
  * [SECT131R2](src/Curve/Binary/SECT131R2.hs)
  * [SECT163K1](src/Curve/Binary/SECT163K1.hs)
  * [SECT163R1](src/Curve/Binary/SECT163R1.hs)
  * [SECT163R2](src/Curve/Binary/SECT163R2.hs)
  * [SECT193R1](src/Curve/Binary/SECT193R1.hs)
  * [SECT193R2](src/Curve/Binary/SECT193R2.hs)
  * [SECT233K1](src/Curve/Binary/SECT233K1.hs)
  * [SECT233R1](src/Curve/Binary/SECT233R1.hs)
  * [SECT239K1](src/Curve/Binary/SECT239K1.hs)
  * [SECT283K1](src/Curve/Binary/SECT283K1.hs)
  * [SECT283R1](src/Curve/Binary/SECT283R1.hs)
  * [SECT409K1](src/Curve/Binary/SECT409K1.hs)
  * [SECT409R1](src/Curve/Binary/SECT409R1.hs)
  * [SECT571K1](src/Curve/Binary/SECT571K1.hs)
  * [SECT571R1](src/Curve/Binary/SECT571R1.hs)

### Edwards curves

* Edwards curves
  * [Curve1174](src/Curve/Edwards/Curve1174.hs)
  * [Curve41417](src/Curve/Edwards/Curve41417.hs)
  * [E-222](src/Curve/Edwards/E222.hs)
  * [E-382](src/Curve/Edwards/E382.hs)
  * [E-521](src/Curve/Edwards/E521.hs)
  * [Ed25519 (Curve25519)](src/Curve/Edwards/Ed25519.hs)
  * [Ed3363 (HighFive)](src/Curve/Edwards/Ed3363.hs)
  * [Ed448 (Goldilocks)](src/Curve/Edwards/Ed448.hs)
  * [JubJub](src/Curve/Edwards/JubJub.hs)

### Montgomery curves

* Montgomery curves
  * [Curve25519 (Ed25519)](src/Curve/Montgomery/Curve25519.hs)
  * [Curve383187](src/Curve/Montgomery/Curve383187.hs)
  * [Curve448 (Goldilocks)](src/Curve/Montgomery/Curve448.hs)
  * [M-221](src/Curve/Montgomery/M221.hs)
  * [M-383](src/Curve/Montgomery/M383.hs)
  * [M-511](src/Curve/Montgomery/M511.hs)

### Weierstrass curves

* [Anomalous](src/Curve/Weierstrass/Anomalous.hs)
* [ANSSI-FRP256V1](src/Curve/Weierstrass/ANSSIFRP256V1.hs)
* Barreto-Lynn-Scott (BLS) curves
  * [BLS12-381](src/Curve/Weierstrass/BLS12_381.hs)
  * [BLS12-381T](src/Curve/Weierstrass/BLS12_381T.hs)
  * [BLS48-581](src/Curve/Weierstrass/BLS48_581.hs)
  * [BLS48-581T](src/Curve/Weierstrass/BLS48_581T.hs)
* Barreto-Naehrig (BN) curves
  * [BN224 (Fp224BN)](src/Curve/Weierstrass/BN224.hs)
  * [BN254 (CurveSNARK)](src/Curve/Weierstrass/BN254.hs)
  * [BN254T (CurveSNARKn2)](src/Curve/Weierstrass/BN254T.hs)
  * [BN254A (Fp254BNa)](src/Curve/Weierstrass/BN254A.hs)
  * [BN254AT (Fp254n2BNa)](src/Curve/Weierstrass/BN254AT.hs)
  * [BN254B (Fp254BNb)](src/Curve/Weierstrass/BN254B.hs)
  * [BN254BT (Fp254n2BNb)](src/Curve/Weierstrass/BN254BT.hs)
  * [BN256 (Fp256BN)](src/Curve/Weierstrass/BN256.hs)
  * [BN384 (Fp384BN)](src/Curve/Weierstrass/BN384.hs)
  * [BN462 (Fp462BN)](src/Curve/Weierstrass/BN462.hs)
  * [BN462T (Fp462n2BN)](src/Curve/Weierstrass/BN462T.hs)
  * [BN512 (Fp512BN)](src/Curve/Weierstrass/BN512.hs)
* Brainpool curves
  * [Brainpool-P160R1](src/Curve/Weierstrass/BrainpoolP160R1.hs)
  * [Brainpool-P160T1](src/Curve/Weierstrass/BrainpoolP160T1.hs)
  * [Brainpool-P192R1](src/Curve/Weierstrass/BrainpoolP192R1.hs)
  * [Brainpool-P192T1](src/Curve/Weierstrass/BrainpoolP192T1.hs)
  * [Brainpool-P224R1](src/Curve/Weierstrass/BrainpoolP224R1.hs)
  * [Brainpool-P224T1](src/Curve/Weierstrass/BrainpoolP224T1.hs)
  * [Brainpool-P256R1](src/Curve/Weierstrass/BrainpoolP256R1.hs)
  * [Brainpool-P256T1](src/Curve/Weierstrass/BrainpoolP256T1.hs)
  * [Brainpool-P320R1](src/Curve/Weierstrass/BrainpoolP320R1.hs)
  * [Brainpool-P320T1](src/Curve/Weierstrass/BrainpoolP320T1.hs)
  * [Brainpool-P384R1](src/Curve/Weierstrass/BrainpoolP384R1.hs)
  * [Brainpool-P384T1](src/Curve/Weierstrass/BrainpoolP384T1.hs)
  * [Brainpool-P512R1](src/Curve/Weierstrass/BrainpoolP512R1.hs)
  * [Brainpool-P512T1](src/Curve/Weierstrass/BrainpoolP512T1.hs)
* SECP (NIST) curves
  * [SECP112R1](src/Curve/Weierstrass/SECP112R1.hs)
  * [SECP112R2](src/Curve/Weierstrass/SECP112R2.hs)
  * [SECP128R1](src/Curve/Weierstrass/SECP128R1.hs)
  * [SECP128R2](src/Curve/Weierstrass/SECP128R2.hs)
  * [SECP160K1](src/Curve/Weierstrass/SECP160K1.hs)
  * [SECP160R1](src/Curve/Weierstrass/SECP160R1.hs)
  * [SECP160R2](src/Curve/Weierstrass/SECP160R2.hs)
  * [SECP192K1](src/Curve/Weierstrass/SECP192K1.hs)
  * [SECP192R1](src/Curve/Weierstrass/SECP192R1.hs)
  * [SECP224K1](src/Curve/Weierstrass/SECP224K1.hs)
  * [SECP224R1](src/Curve/Weierstrass/SECP224R1.hs)
  * [SECP256K1](src/Curve/Weierstrass/SECP256K1.hs)
  * [SECP256R1](src/Curve/Weierstrass/SECP256R1.hs)
  * [SECP384R1](src/Curve/Weierstrass/SECP384R1.hs)
  * [SECP521R1](src/Curve/Weierstrass/SECP521R1.hs)
