import myskibidiacademia/[congruence, interfaces]

# see test_Zn for most basic tests

block:
  type Weird = Mod[float, 7.35]
  let zero = Weird.from(0)
  let one = Weird.from(1)
  doAssert zero == Weird.from(7.35)
  doAssert one == Weird.from(8.35)
  let two = one + one
  doAssert two == Weird.from(2)
  doAssert two == Weird.from(9.35)
  let four = two + two
  let eight = four + four
  doAssert eight == Weird.from(8)
  doAssert eight == Weird.from(8 - 7.35)
  doAssert float(eight) == (8 - 7.35)
  doAssert two * four == eight
  doAssert mulBinary(two, four) == eight
  block: # these don't make sense
    #doAssert not invertible(zero) # it thinks gcd(0, 7.35) is 7.35 which we don't reduce
    doAssert invertible(one)
    doAssert invertible(two)
    doAssert invertible(four)
    doAssert invertible(eight)
    doAssert inverse(one) == one
    doAssert inverse(two) == two
    doAssert inverse(four) == four
    doAssert inverse(eight) == eight

# todo test polynomials, in both ways
