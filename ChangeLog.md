# Change log for elliptic-curve

## 0.3.0

* Refactor library structure from `Curve` to `Data.Curve`.
* Remove `Group` class and `BN254TF` field group.
* Fix dependency issue with `galois-field`.
* Add `BN254C` and `BN254D` elliptic curves.
* Add `frob` function for elliptic curves.

## 0.2.2

* Fix `Arbitrary` instances for elliptic curves.
* Fix `Random` instances for elliptic curves.
* Fix `Functor` instances for groups.

## 0.2.1

* Fix dependency issue with `galois-field`.

## 0.2.0

* Add `Arbitrary` instances for elliptic curves.
* Add `Pretty` instances for elliptic curves.
* Add `Random` instances for elliptic curves.
* Add `Group` class for elliptic curves.
* Add `Field` group under the `Group` class.
* Add `BN254TF` field group under the `Field` form.
* Add `Projective` coordinates for `Binary` curves.
* Add `Projective` coordinates for `Edwards` curves.
* Add `Jacobian` coordinates for `Weierstrass` curves.
* Add `Projective` coordinates for `Weierstrass` curves.
* Add `char` function for elliptic curves.
* Add `cof` function for elliptic curves.
* Add `disc` function for elliptic curves.
* Add `def` function for groups.
* Add `gen` function for groups.
* Add `order` function for groups.
* Add `mul` function over exponent prime fields.
* Add `mul'` function over integers.
* Add `point` function for elliptic curves.
* Add `pointX` function for elliptic curves.
* Add `fromA` function for elliptic curves.
* Add `toA` function for elliptic curves.

## 0.1.1

* Fix dependency issue with `galois-field`.

## 0.1.0

* Initial release.
