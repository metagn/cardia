template `div`*(a, b: float): float = float(int(a / b))

type # maybe make these peano later
  Zero*[T] = object
  One*[T] = object

# for compiler performance might want to use knot for these
template value*[T: SomeNumber | SomeOrdinal](x: Zero[T]): T = T(0)
template value*[T: SomeNumber | SomeOrdinal](x: One[T]): T = T(1)

template zero*[T](_: typedesc[T]): T =
  mixin value
  value(Zero[T]())

template one*[T](_: typedesc[T]): T =
  mixin value
  value(One[T]())

template isZero*[T](a: T): bool =
  a == zero(T)

template isOne*[T](a: T): bool =
  a == one(T)

template isNonZero*[T](a: T): bool =
  a != zero(T)

type IsUnit*[T] = object

template apply*[T: SomeSignedInt](_: IsUnit[T], val: T): bool =
  val == -1 or val == 1

template apply*[T: SomeUnsignedInt](_: IsUnit[T], val: T): bool =
  val == 1

template apply*[T: SomeFloat](_: IsUnit[T], val: T): bool =
  val != 0

template isUnit*[T](x: T): bool =
  mixin apply
  apply(IsUnit[T](), x)

type IsNonNegativeUnit*[T] = object
  ## misleading, just checks if value that is known to be non-negative is unit

template apply*[T](_: IsNonNegativeUnit[T], val: T): bool =
  # so we can do this
  apply(IsUnit[T](), val)

template apply*[T: SomeSignedInt](_: IsNonNegativeUnit[T], val: T): bool =
  val == 1

template apply*[T: SomeUnsignedInt](_: IsNonNegativeUnit[T], val: T): bool =
  val == 1

template apply*[T: SomeFloat](_: IsNonNegativeUnit[T], val: T): bool =
  val != 0

template isNonNegativeUnit*[T](x: T): bool =
  mixin apply
  apply(IsNonNegativeUnit[T](), x)
