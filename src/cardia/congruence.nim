import ./interfaces, std/math

type Mod*[T; N: static T] = distinct T

template `==`*[T; N: static T](a, b: Mod[T, N]): bool = T(a) == T(b)
template `$`*[T; N: static T](a: Mod[T, N]): string = $T(a)

template value*[T; N: static T](x: Zero[Mod[T, N]]): Mod[T, N] = Mod[T, N](zero(T))
template value*[T; N: static T](x: One[Mod[T, N]]): Mod[T, N] = Mod[T, N](one(T))

proc toMod*[T; N: static T](x: T): Mod[T, N] {.inline.} =
  Mod[T, N](euclMod(x, N))

template `from`*[T; N: static T](_: type Mod[T, N], val: static T): Mod[T, N] =
  when val >= zero(T) and val < N:
    Mod[T, N](val)
  else:
    toMod[T, N](val)

template `from`*[T; N: static T](_: type Mod[T, N], val: T): Mod[T, N] =
  toMod[T, N](val)

proc `+=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]) {.inline.} =
  # hopefully faster than mod
  T(a) += T(b)
  if T(a) >= N: T(a) -= N

template `+`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  var x = a
  x += b
  x

proc `-`*[T; N: static T](a: Mod[T, N]): Mod[T, N] {.inline.} =
  if T(a).isZero:
    result = a
  else:
    result = Mod[T, N](N - T(a))

proc `-=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]) {.inline.} =
  # fully inlined a + (-b), hopefully faster than mod
  T(a) += N - T(b)
  if T(a) >= N: T(a) -= N

template `-`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  var x = a
  x -= b
  x

template mulRaw*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  toMod[T, N](T(a) * T(b))

proc mulBinaryOrdered*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  # b should be smaller
  # thought about montgomery 
  let tail = T(b) mod 2
  if tail == 1:
    result = a
  else:
    when T is SomeInteger:
      result = Mod[T, N](zero(T))
    else:
      # floats shouldn't use this but still safeguard
      # hopefully compiler optimizes out impossibilities
      if tail > 1:
        result = a + Mod[T, N]((tail - one(T)) * T(a))
      else:
        result = Mod[T, N](tail * T(a))
  var a = T(a)
  var b = T(b) div 2 # integer
  while b.isNonZero:
    # emulate a += a:
    when compiles(a shl 1):
      a = a shl 1
    else:
      a = a + a
    if a >= N: a -= N
    if (b mod 2).isNonZero: # now it has to be 1
      result += Mod[T, N](a)
    b = b div 2

proc mulBinary*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] {.inline.} =
  if T(a) >= T(b):
    mulBinaryOrdered(a, b)
  else:
    mulBinaryOrdered(b, a)

type PreferRawMultiplication*[T] = object
template value*[T; N: static T](x: PreferRawMultiplication[Mod[T, N]]): bool =
  # default
  false
template value*[T: SomeFloat; N: static T](x: PreferRawMultiplication[Mod[T, N]]): bool =
  true
template value*[T: SomeInteger; N: static T](x: PreferRawMultiplication[Mod[T, N]]): bool =
  N <= 32767

template `*`*[T; N: static T](a, b: Mod[T, N]): Mod[T, N] =
  when value(PreferRawMultiplication[Mod[T, N]]()):
    mulRaw(a, b)
  else:
    mulBinary(a, b)

template `*=`*[T; N: static T](a: var Mod[T, N], b: Mod[T, N]) =
  a = a * b

proc invertible*[T; N: static T](a: Mod[T, N]): bool =
  gcd(T(a), N).isNonNegativeUnit

proc inverse*[T; N: static T](a: Mod[T, N]): Mod[T, N] =
  # https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
  if T(a).isNonNegativeUnit: return a
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
