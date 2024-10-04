import cardia/[congruence, interfaces]

# todo test exponentiation

block: # Z1, trivial group, zero ring
  type Z1 = Mod[int, 1]
  let zero = Z1.from(0)
  doAssert zero == Z1.from(1)
  doAssert zero == zero(Z1)
  doAssert zero + zero == zero
  doAssert zero - zero == zero
  doAssert zero * zero == zero
  doAssert mulBinary(zero, zero) == zero
  doAssert invertible(zero)
  doAssert inverse(zero) == zero

block: # Z2
  type Z2 = Mod[int, 2]
  let zero = Z2.from(0)
  let one = Z2.from(1)
  doAssert Z2.from(2) == zero
  doAssert Z2.from(3) == one
  doAssert Z2.from(-1) == one
  doAssert Z2.from(-2) == zero
  doAssert zero != one
  doAssert zero == zero(Z2)
  doAssert one == one(Z2)
  doAssert zero + one == one
  doAssert one + zero == one
  doAssert one + one == zero
  doAssert one - zero == one
  doAssert one - one == zero
  doAssert zero - zero == zero
  doAssert zero - one == one
  doAssert one * one == one
  doAssert one * zero == zero
  doAssert zero * zero == zero
  doAssert zero * one == zero
  doAssert mulBinary(one, one) == one
  doAssert mulBinary(one, zero) == zero
  doAssert mulBinary(zero, zero) == zero
  doAssert mulBinary(zero, one) == zero
  doAssert not invertible(zero)
  doAssert invertible(one)
  doAssert inverse(one) == one

block: # Z3
  type Z3 = Mod[int, 3]
  let zero = Z3.from(0)
  let one = Z3.from(1)
  let two = Z3.from(2)
  doAssert zero != one
  doAssert one != two
  doAssert zero != two
  doAssert zero == zero(Z3)
  doAssert one == one(Z3)
  doAssert one + one == two
  doAssert two + one == zero
  doAssert two + two == one
  doAssert one - two == two
  doAssert two - one == one
  doAssert two * two == one
  doAssert mulBinary(two, two) == one
  doAssert not invertible(zero)
  doAssert invertible(one)
  doAssert invertible(two)
  doAssert inverse(one) == one
  doAssert inverse(two) == two

proc additionTable[T; N: static T](M: type Mod[T, N]): array[N, array[N, T]] =
  for i in 0 ..< N:
    for j in 0 ..< N:
      result[i][j] = T(M(i) + M(j))

proc multiplicationTable[T; N: static T](M: type Mod[T, N]): array[N, array[N, T]] =
  for i in 0 ..< N:
    for j in 0 ..< N:
      result[i][j] = T(M(i) * M(j))

proc binaryMultiplicationTable[T; N: static T](M: type Mod[T, N]): array[N, array[N, T]] =
  for i in 0 ..< N:
    for j in 0 ..< N:
      result[i][j] = T(mulBinary(M(i), M(j)))

proc inverses[T; N: static T](M: type Mod[T, N]): array[N, T] =
  for i in 0 ..< N:
    if invertible(M(i)):
      result[i] = T(inverse(M(i)))
    else:
      result[i] = zero(T)

block: # Z4
  type Z4 = Mod[int, 4]
  doAssert additionTable(Z4) == [
    [0, 1, 2, 3],
    [1, 2, 3, 0],
    [2, 3, 0, 1],
    [3, 0, 1, 2]
  ]
  let multab = multiplicationTable(Z4)
  doAssert multab == [
    [0, 0, 0, 0],
    [0, 1, 2, 3],
    [0, 2, 0, 2],
    [0, 3, 2, 1]
  ]
  doAssert multab == binaryMultiplicationTable(Z4)
  doAssert inverses(Z4) == [0, 1, 0, 3]

block: # Z5
  type Z5 = Mod[int, 5]
  doAssert additionTable(Z5) == [
    [0, 1, 2, 3, 4],
    [1, 2, 3, 4, 0],
    [2, 3, 4, 0, 1],
    [3, 4, 0, 1, 2],
    [4, 0, 1, 2, 3]
  ]
  let multab = multiplicationTable(Z5)
  doAssert multab == [
    [0, 0, 0, 0, 0],
    [0, 1, 2, 3, 4],
    [0, 2, 4, 1, 3],
    [0, 3, 1, 4, 2],
    [0, 4, 3, 2, 1]
  ]
  doAssert multab == binaryMultiplicationTable(Z5)
  doAssert inverses(Z5) == [0, 1, 3, 2, 4]

block: # Z6
  type Z6 = Mod[int, 6]
  doAssert additionTable(Z6) == [
    [0, 1, 2, 3, 4, 5],
    [1, 2, 3, 4, 5, 0],
    [2, 3, 4, 5, 0, 1],
    [3, 4, 5, 0, 1, 2],
    [4, 5, 0, 1, 2, 3],
    [5, 0, 1, 2, 3, 4]
  ]
  let multab = multiplicationTable(Z6)
  doAssert multab == [
    [0, 0, 0, 0, 0, 0],
    [0, 1, 2, 3, 4, 5],
    [0, 2, 4, 0, 2, 4],
    [0, 3, 0, 3, 0, 3],
    [0, 4, 2, 0, 4, 2],
    [0, 5, 4, 3, 2, 1]
  ]
  doAssert multab == binaryMultiplicationTable(Z6)
  doAssert inverses(Z6) == [0, 1, 0, 0, 0, 5]

block: # Z8
  type Z8 = Mod[int, 8]
  let multab = multiplicationTable(Z8)
  doAssert multab == [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 2, 3, 4, 5, 6, 7],
    [0, 2, 4, 6, 0, 2, 4, 6],
    [0, 3, 6, 1, 4, 7, 2, 5],
    [0, 4, 0, 4, 0, 4, 0, 4],
    [0, 5, 2, 7, 4, 1, 6, 3],
    [0, 6, 4, 2, 0, 6, 4, 2],
    [0, 7, 6, 5, 4, 3, 2, 1]
  ]
  doAssert multab == binaryMultiplicationTable(Z8)
  doAssert inverses(Z8) == [0, 1, 0, 3, 0, 5, 0, 7]
