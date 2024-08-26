import myskibidiacademia/polynomials

proc `*!`[T](a, b: Polynomial[T]): Polynomial[T] =
  mulKaratsubaSingle(a.coefficients, b.coefficients)
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
