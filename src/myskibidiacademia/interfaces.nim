type
  Zero*[T] = object
  One*[T] = object

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
