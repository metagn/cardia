# todo: conversion from N!, checking 1 to 1 for lookup arrays and arbitrary functions
# shift, negation permutations on all integers
# deduce smallest int type for const permutation
# construction from cycles dont know most efficient way

import util/boolseq

type
  Permutation*[R] = object
    values*: array[R, R]
  NPermutation*[N: static int] = Permutation[range[0..N-1]]
  FinitaryPermutation* = object
    values*: seq[int]
  AnyPermutation* = Permutation | FinitaryPermutation

template Range*[I](p: typedesc[Permutation[I]]): typedesc = I
template Range*(p: typedesc[FinitaryPermutation]): typedesc = int
template Range*[I](p: Permutation[I]): typedesc = I
template Range*(p: FinitaryPermutation): typedesc = int

proc permutation*[I](lookup: array[I, I],
  check: static bool = compileOption("rangechecks")): Permutation[I] =
  when check:
    var set = initBoolSeq(lookup.len)
  for i in low(lookup) .. high(lookup):
    let val = lookup[i]
    when check:
      let ind = val - low(I)
      if ind in set:
        raise newException(ValueError, "value " & $val & " encountered a second time at " & $i & " in " & $lookup)
      else:
        set.incl(ind)
    result.values[i] = val

proc toBijection*[I](lookup: array[I, int],
  check: static bool = compileOption("rangechecks")): array[I, I] {.inline.} =
  when check:
    for i in low(I) .. high(I):
      result[i] = lookup[i]
  else:
    array[I, I](lookup)

proc map*[I](p: Permutation[I], ind: I): I {.inline.} =
  p.values[ind]

proc map*(p: FinitaryPermutation, ind: int): int {.inline.} =
  if ind < p.values.len:
    p.values[ind]
  else:
    ind

proc apply*[I](c: var openarray[I], p: Permutation[I]) =
  for i in 0 ..< c.len:
    c[i] = p.values[c[i]]

proc inverse*[I](p: Permutation[I], ind: I): I {.inline.} =
  # O(N)
  p.values.find(ind)

proc inverse*(p: FinitaryPermutation, ind: int): int {.inline.} =
  # O(N)
  if ind < p.values.len:
    p.values.find(ind)
  else:
    ind

proc inverse*[I](p: Permutation[I]): Permutation[I] {.inline.} =
  # O(N)
  for i in low(I) .. high(I):
    result.values[p.values[i]] = i

proc inverse*(p: FinitaryPermutation): FinitaryPermutation {.inline.} =
  # O(N)
  result.values = newSeq[int](p.values.len)
  for i in 0 ..< p.values.len:
    result.values[p.values[i]] = i

proc upcastShifted*[I, J](result: var Permutation[J], p: Permutation[I], start: J = low(J)) =
  static: doAssert (high(J) - low(J)) >= (high(I) - low(I))
  for i in low(I) .. high(I):
    result.values[i + (low(J) - low(I))] = p.values[i]

proc upcastShifted*[I, J](p: Permutation[I], start: J = low(J)): Permutation[J] {.inline.} =
  upcastShifted(result, p, start)

proc upcast*[I, J](result: var Permutation[J], p: Permutation[I]) =
  static: doAssert high(J) >= high(I) and low(J) <= low(I) 
  for i in low(I) .. high(I):
    result.values[i] = p.values[i]

proc upcast*[I, J](p: Permutation[I]): Permutation[J] {.inline.} =
  upcast(result, p)

proc composeInPlace*[I](a: Permutation[I], b: var Permutation[I]) =
  ## turns b(x) into a(b(x))
  for i in low(I) .. high(I):
    # can't modify a.values
    b.values[i] = a.values[b.values[i]]

proc compose*[I](a, b: Permutation[I]): Permutation[I] =
  result = b
  composeInPlace(a, result)

proc composeInPlace*(a: FinitaryPermutation, b: var FinitaryPermutation) =
  ## turns b(x) into a(b(x))
  if b.values.len < a.values.len:
    let oldLen = b.values.len
    b.values.setLen(a.values.len)
    for i in oldLen ..< b.values.len:
      b.values[i] = i
  for i in 0 ..< b.values.len:
    # can't modify a.values
    b.values[i] = a.values[b.values[i]]

proc compose*(a, b: FinitaryPermutation): FinitaryPermutation =
  result = b
  composeInPlace(a, result)

iterator cycle*[I](p: Permutation[I], ind: I): I =
  var res = ind
  while true:
    yield res
    res = p.values[res]
    if res == ind: break

iterator cycle*(p: FinitaryPermutation, ind: int): int =
  var res = ind
  while true:
    yield res
    res = p.map(res)
    if res == ind: break

iterator cycleExclusive*[I](p: Permutation[I], ind: I): I =
  var res = ind
  while true:
    res = p.values[res]
    if res == ind: break
    yield res

iterator cycleExclusive*(p: FinitaryPermutation, ind: int): int =
  var res = ind
  while true:
    res = p.map(res)
    if res == ind: break
    yield res

proc `$`*[I](p: Permutation[I]): string =
  var set = initBoolSeq(p.values.len)
  for i in low(I) .. high(I):
    let ind = i - low(I)
    if ind in set:
      continue
    else:
      set.incl(ind)
    if p.map(i) == i: continue
    result.add('(')
    result.add($i)
    for c in cycleExclusive(p, i):
      set.incl(c - low(I))
      result.add(' ')
      result.add($c)
    result.add(')')
  if result.len == 0: result = "(0)"
