# needs tests, maybe more stuff

import ./interfaces, std/math

type Mod*[T; N: static T] = distinct T

template value*[T; N: static T](x: Zero[Mod[T, N]]): Mod[T, N] = zero(T)
template value*[T; N: static T](x: One[Mod[T, N]]): Mod[T, N] = one(T)

proc toMod*[T; N: static T](x: T): Mod[T, N] {.inline.} =
  Mod[T, N](euclMod(x, N))

template `+`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  toMod(T(a) + T(b))

template `-`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  toMod(T(a) - T(b))

template `*`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  # karatsuba/montgomery for large N
  toMod(T(a) * T(b))

template `+=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]): Mod[T, N] =
  a = toMod(T(a) + T(b))

template `-=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]): Mod[T, N] =
  a = toMod(T(a) - T(b))

template `*=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]): Mod[T, N] =
  # karatsuba/montgomery for large N
  a = toMod(T(a) * T(b))

template `-`*[T; N: static T](a: Mod[T, N]): Mod[T, N] =
  Mod[T, N](N - T(a))

proc invertible*[T; N: static T](a: Mod[T, N]): Mod[T, N] =
  gcd(T(a), N).isOne

proc inverse*[T; N: static T](a: Mod[T, N]): Mod[T, N] =
  # https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
  var res = zero(T)
  var newRes = one(T)
  var rem = N
  var newRem = T(a)
  while newRem.isNonZero:
    let q = rem div newRem
    (res, newRes) = (newRes, res - q * newRes)
    (rem, newRem) = (newRem, rem - q * newRem)
  doAssert rem <= one(T), $a & " not invertible mod " & $N
  result = toMod[T, N](res)

proc `^`*[T; N: static T](a: Mod[T, N], n: int): Mod[T, N] =
  # https://en.wikipedia.org/wiki/Exponentiation_by_squaring#With_constant_auxiliary_memory
  var n = n
  if n < 0:
    result = inverse(a)
    n = -n
  elif n == 0:
    return one(Mod[T, N])
  else:
    result = a
  var y = one(Mod[T, N])
  while n > 1:
    if n mod 2 != 0:
      y *= x
      dec n
    x *= x
    n = n div 2
  result = x * y
