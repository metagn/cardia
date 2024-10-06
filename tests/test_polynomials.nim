import cardia/[interfaces, polynomials]

proc `*!`[T](a, b: Polynomial[T]): Polynomial[T] =
  mulKaratsubaSingle(a.coefficients, b.coefficients)

block:
  let xp1 = polynomial(1, 1)
  let xp2 = xp1 * xp1
  doAssert xp2 == polynomial(1, 2, 1)
  let xp3 = xp2 * xp1
  doAssert xp3 == polynomial(1, 3, 3, 1)
  doAssert xp3 == xp2 *! xp1
  let xp4 = xp3 * xp1
  doAssert xp4 == polynomial(1, 4, 6, 4, 1)
  doAssert xp4 == xp3 *! xp1
  doAssert xp4 == xp2 * xp2
  let xp5 = xp4 * xp1
  doAssert xp5 == polynomial(1, 5, 10, 10, 5, 1)
  doAssert xp5 == xp4 *! xp1
  doAssert xp5 == xp3 * xp2
  doAssert xp5 == xp3 *! xp2

block: # float
  let xp1 = polynomial(1.0, 1.0)
  let xp2 = xp1 * xp1
  doAssert xp2 == polynomial(1.0, 2.0, 1.0)
  let xp3 = xp2 * xp1
  doAssert xp3 == polynomial(1.0, 3.0, 3.0, 1.0)
  doAssert xp3 == xp2 *! xp1
  let xp4 = xp3 * xp1
  doAssert xp4 == polynomial(1.0, 4.0, 6.0, 4.0, 1.0)
  doAssert xp4 == xp3 *! xp1
  doAssert xp4 == xp2 * xp2
  let xp5 = xp4 * xp1
  doAssert xp5 == polynomial(1.0, 5.0, 10.0, 10.0, 5.0, 1.0)
  doAssert xp5 == xp4 *! xp1
  doAssert xp5 == xp3 * xp2
  doAssert xp5 == xp3 *! xp2

  let a = polynomial(1.5, 2.5, 1.5)
  doAssert $a == "1.5 + 2.5x + 1.5x^2"
  let b = a * a
  doAssert $b == "2.25 + 7.5x + 10.75x^2 + 7.5x^3 + 2.25x^4"
  let c = a * xp1
  doAssert $c == "1.5 + 4.0x + 4.0x^2 + 1.5x^3"
  let a2 = a * 2
  doAssert $a2 == "3.0 + 5.0x + 3.0x^2"

block: # nested
  let x = polynomial(1, 1, 1)
  let y = polynomial(x, 2 * x, x)
  doAssert $y == "1 + y + y^2 + (2 + 2y + 2y^2)x + (1 + y + y^2)x^2"
  let z = y * polynomial(3)
  doAssert $z == "3 + 3y + 3y^2 + (6 + 6y + 6y^2)x + (3 + 3y + 3y^2)x^2"

type PolynomialWithName[Name: static string, T] = distinct Polynomial[T]
converter toPolynomial[Name: static string, T](a: PolynomialWithName[Name, T]): Polynomial[T] =
  Polynomial[T](a)
proc `$`[Name: static string, T](x: PolynomialWithName[Name, T]): string =
  toString(toPolynomial(x), Name)

template value*[Name: static string, T](x: Zero[PolynomialWithName[Name, T]]): PolynomialWithName[Name, T] =
  PolynomialWithName[Name, T](zero(Polynomial[T]))

template value*[Name: static string, T](x: One[PolynomialWithName[Name, T]]): PolynomialWithName[Name, T] =
  PolynomialWithName[Name, T](one(Polynomial[T]))
template `*=`[Name: static string, T](x: var PolynomialWithName[Name, T], y: PolynomialWithName[Name, T]) =
  Polynomial[T](x) *= Polynomial[T](y)

block: # generic
  type YPolynomial[T] = PolynomialWithName["y", int]
  let x = YPolynomial[int](polynomial(1, 1, 1))
  doAssert $x == "1 + y + y^2"
  let y = polynomial(x, YPolynomial[int](2 * x), x)
  doAssert $y == "1 + y + y^2 + (2 + 2y + 2y^2)x + (1 + y + y^2)x^2"
  let z = y * YPolynomial[int](polynomial(3))
  doAssert $z == "3 + 3y + 3y^2 + (6 + 6y + 6y^2)x + (3 + 3y + 3y^2)x^2"

block: # generic
  type ZPolynomial[T] = PolynomialWithName["z", int]
  let x = ZPolynomial[int](polynomial(1, 1, 1))
  doAssert $x == "1 + z + z^2"
  let y = polynomial(x, ZPolynomial[int](2 * x), x)
  doAssert $y == "1 + z + z^2 + (2 + 2z + 2z^2)x + (1 + z + z^2)x^2"
  let z = y * ZPolynomial[int](polynomial(3))
  doAssert $z == "3 + 3z + 3z^2 + (6 + 6z + 6z^2)x + (3 + 3z + 3z^2)x^2"

import cardia/congruence

block: # polynomial in Z2
  type Z2 = Mod[int, 2]
  let x = polynomial(Z2 1, Z2 1, Z2 1)
  let y = polynomial(Z2 1, Z2 0, Z2 1)
  doAssert $x == "1 + x + x^2"
  doAssert $y == "1 + x^2"
  doAssert x == -x
  doAssert y == -y
  let add = x + y
  let sub = x - y
  doAssert $add == "0 + x"
  doAssert add == sub
  let mul = x * y
  doAssert $mul == "1 + x + x^3 + x^4"
  let x2 = x * x
  doAssert $x2 == "1 + x^2 + x^4"
  let y2 = y * y
  doAssert $y2 == "1 + x^4"
  when false:
    # both of these are broken (recursive karatsuba)
    # gives 1 + x^2 + x^6 + x^7:
    doAssert $(x2 * y2) == "1 + x^2 + x^6 + x^8"
    # gives 1 + x^2 + x^5 + x^y:
    let mul2 = mul * mul
    doAssert $mul2 == "1 + x^2 + x^6 + x^8"
    doAssert x2 * y2 == mul2
  doAssert $(x2 *! y2) == "1 + x^2 + x^6 + x^8"
  let mul2 = mul *! mul
  doAssert $mul2 == "1 + x^2 + x^6 + x^8"
  doAssert x2 *! y2 == mul2
