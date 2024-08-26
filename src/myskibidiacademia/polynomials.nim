type
  Polynomial*[T] = object
    coefficients*: seq[T]

proc `$`*[T](a: Polynomial[T]): string =
  let len = a.coefficients.len
  if len == 0:
    return $T(0)
  result = $a.coefficients[0]
  for i in 1 ..< len:
    let ai = a.coefficients[i]
    if ai == 0: continue
    result.add(" + ")
    result.add($ai)
    if i == 1:
      result.add("x")
    else:
      result.add("x^")
      result.add($i)

proc polynomial*[T](x: varargs[T]): Polynomial[T] {.inline.} =
  Polynomial[T](coefficients: @x)

proc zero*[T](): Polynomial[T] {.inline.} =
  Polynomial[T](coefficients: @[])

proc one*[T](): Polynomial[T] {.inline.} =
  polynomial[T](1)

proc X*[T](factor = 1): Polynomial[T] {.inline.} =
  result = Polynomial[T](coefficients: newSeq[T](factor + 1))
  result.coefficients[factor] = 1

proc polynomialTimesXN*[T](factor: int, x: varargs[T]): Polynomial[T] {.inline.} =
  result = Polynomial[T](coefficients: newSeq[T](x.len + factor))
  for i in 0 ..< x.len:
    result.coefficients[i + factor] = x[i]

proc deg*[T](a: Polynomial[T]): int {.inline.} =
  high(a.coefficients)

proc updateDegree*[T](a: var Polynomial[T]) =
  let origLast = high(a.coefficients)
  var last = origLast
  while last >= 0:
    if a.coefficients[last] != 0:
      if last != origLast:
        # branching better than pointless function call
        a.coefficients.setLen(last + 1)
      return
    dec last
  # all zero
  a.coefficients = @[]

proc `+=`*[T](a: var Polynomial[T], b: Polynomial[T]) =
  if b.coefficients.len == 0: return
  if b.coefficients.len > a.coefficients.len:
    a.coefficients.setLen(b.coefficients.len)
  for i in 0 ..< b.coefficients.len:
    a.coefficients[i] += b.coefficients[i]
  # we can do this above but maybe help vectorization
  updateDegree(a)

proc `+`*[T](a, b: Polynomial[T]): Polynomial[T] =
  if a.coefficients.len > b.coefficients.len:
    result = a
    result += b
  else:
    result = b
    result += a

proc negate*[T](a: var Polynomial[T]) =
  for i in 0 ..< a.coefficients.len:
    a.coefficients[i] = -a.coefficients[i]

proc `-`*[T](a: Polynomial[T]): Polynomial[T] =
  result = a
  negate(result)

proc `-=`*[T](a: var Polynomial[T], b: Polynomial[T]) =
  if b.coefficients.len == 0: return
  if b.coefficients.len > a.coefficients.len:
    a.coefficients.setLen(b.coefficients.len)
  for i in 0 ..< b.coefficients.len:
    a.coefficients[i] -= b.coefficients[i]
  # we can do this above but maybe help vectorization
  updateDegree(a)

proc `-`*[T](a, b: Polynomial[T]): Polynomial[T] =
  if a.coefficients.len > b.coefficients.len:
    result = a
    result -= b
  else:
    result = b
    result -= a

proc `*=`*[T](a: var Polynomial[T], scalar: T) =
  if scalar == 1: return
  for i in 0 ..< a.coefficients.len:
    a.coefficients[i] *= scalar

proc `*`*[T](a: Polynomial[T], scalar: T): Polynomial[T] =
  result = a
  result *= scalar

template `*`*[T](scalar: T, a: Polynomial[T]): Polynomial[T] =
  a * scalar

template safeAdd[T](x: var T, a: openarray[T], i: int) =
  if i < a.len:
    x += a[i]

template safeSub[T](x: var T, a: openarray[T], i: int) =
  if i < a.len:
    x -= a[i]

proc mulKaratsubaSingle*[T](a, b: openarray[T]): Polynomial[T] =
  # from http://www.weimerskirch.org/files/Weimerskirch_KAA.pdf
  let
    nA = a.len
    nB = b.len
  if nB == 0: return polynomial(b)
  if nA == 0: return polynomial(a)
  if nB == 1: return polynomial(a) * b[0]
  if nA == 1: return polynomial(b) * a[0]
  let
    minN = min(nA, nB)
    maxN = max(nA, nB)
  var Di = newSeq[T](minN)
  for i in 0 ..< minN:
    Di[i] = a[i] * b[i]
  let stLen = nA + nB - 3
  var Dst = newSeq[T](stLen)
  for i in 0 ..< stLen:
    let actualI = i + 1
    var val = default(T)
    for t in (actualI div 2 + 1) .. min(actualI, maxN - 1):
      let s = actualI - t
      var bPart = default(T)
      safeAdd(bPart, b, s)
      safeAdd(bPart, b, t)
      if bPart != 0:
        # b more likely to be smaller, check bPart first
        var aPart = default(T)
        safeAdd(aPart, a, s)
        safeAdd(aPart, a, t)
        val += aPart * bPart
    Dst[i] = val
  result = Polynomial[T](coefficients: newSeq[T](nA + nB - 1))
  result.coefficients[0] = Di[0]
  result.coefficients[nA + nB - 2] = a[nA - 1] * b[nB - 1]
  for i in 1 ..< nA + nB - 2:
    var val = Dst[i - 1]
    for t in (i div 2 + 1) .. min(i, maxN - 1):
      let s = i - t
      safeSub(val, Di, s)
      safeSub(val, Di, t)
    if i mod 2 == 0:
      safeAdd(val, Di, i div 2)
    result.coefficients[i] = val

proc mulKaratsubaRec*[T](a, b: openarray[T]): Polynomial[T] =
  let
    degA = a.len - 1
    degB = b.len - 1
  if degA < 2 or degB < 2 or (degA < 4 and degB < 4):
    return mulKaratsubaSingle(a, b)
  if degA < degB:
    return mulKaratsubaRec(b, a)
  let N = degA + 1
  let split = N div 2
  template aUpper: untyped = a.toOpenArray(split + 1, degA)
  template aLower: untyped = a.toOpenArray(0, split)
  template bUpper: untyped = b.toOpenArray(split + 1, degB)
  template bLower: untyped = b.toOpenArray(0, split)
  if degB < split + 1:
    # bUpper = 0
    let
      D0 = mulKaratsubaRec(aLower, b)
      D01mD0 = mulKaratsubaRec(aUpper, b)
    result = polynomialTimesXN(factor = split + 1, D01mD0.coefficients)
    result += D0
  else:
    var
      D0 = mulKaratsubaRec(aLower, bLower)
      D1 = mulKaratsubaRec(aUpper, bUpper)
      D01 = mulKaratsubaRec(
        (polynomial(aLower) + polynomial(aUpper)).coefficients,
        (polynomial(bLower) + polynomial(bUpper)).coefficients)
    result = polynomialTimesXN(factor = N, D1.coefficients)
    D01 -= D0
    D01 -= D1
    result += polynomialTimesXN(factor = split + 1, D01.coefficients)
    result += D0

proc `*`*[T](a, b: Polynomial[T]): Polynomial[T] =
  mulKaratsubaRec(a.coefficients, b.coefficients)
