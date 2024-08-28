import myskibidiacademia/permutations

block: # just S2
  let default = toBijection([0, 1])
  var x = default
  let id = permutation(default)
  x.apply(id)
  doAssert x == default
  let inverted = toBijection([1, 0])
  let transp = permutation(inverted)
  x.apply(transp)
  doAssert x == inverted
  x.apply(transp)
  doAssert x == default
  let transp2 = compose(id, transp)
  let transp3 = compose(transp, id)
  doAssert transp == transp2
  doAssert transp2 == transp3
  let id2 = compose(transp, transp)
  doAssert id == id2
  let yInitial = [Range(id) 0, 0, 1, 0, 1, 1, 0]
  var y = yInitial
  y.apply(id)
  doAssert y == yInitial
  y.apply(transp)
  doAssert y == [Range(id) 1, 1, 0, 1, 0, 0, 1]
  var values: seq[int]
  for a in cycle(id, 1):
    values.add(a)
  doAssert values == @[1]
  values = @[]
  for a in cycle(transp, 1):
    values.add(a)
  doAssert values == @[1, 0]
  values = @[]
  for a in cycle(id, 0):
    values.add(a)
  doAssert values == @[0]
  values = @[]
  for a in cycle(transp, 0):
    values.add(a)
  doAssert values == @[0, 1]
  doAssert $id == "(0)", $id
  doAssert $transp == "(0 1)", $transp

block: # long permutation
  # (0 6) (1 3) (2 4 7)
  type S8 = range[0..7]
  let p1 = permutation([S8 6, 3, 4, 1, 7, 5, 0, 2])
  doAssert $p1 == "(0 6)(1 3)(2 4 7)"
  var x = [S8 7, 6, 5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5, 6, 7]
  x.apply(p1)
  doAssert x == [S8 2, 0, 5, 7, 1, 4, 3, 6, 3, 4, 1, 7, 5, 0, 2]
  var p = p1
  composeInPlace(p1, p)
  doAssert $p == "(2 7 4)"
  composeInPlace(p1, p)
  doAssert $p == "(0 6)(1 3)"
  composeInPlace(p1, p)
  doAssert $p == "(2 4 7)"
  composeInPlace(p1, p)
  doAssert $p == "(0 6)(1 3)(2 7 4)"
  doAssert p == inverse(p1)
  composeInPlace(p1, p)
  doAssert $p == "(0)"
  composeInPlace(p1, p)
  doAssert p == p1
