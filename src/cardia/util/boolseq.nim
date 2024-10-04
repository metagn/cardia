type BoolSeq* = object
  ## cuts size of seq[bool] to 1/8
  ## dense bitset in 0..<len
  len*: int
  data: seq[byte]

template byteCount(len: int): int = (len + 7) div 8
template byteIndex(ind: int): int = ind div 8
template byteOffset(ind: int): int = ind mod 8
template byteMask(ind: int): byte = 1.byte shl byteOffset(ind)

proc initBoolSeq*(): BoolSeq {.inline.} =
  result = BoolSeq(len: 0, data: @[])

proc initBoolSeq*(len: int): BoolSeq {.inline.} =
  result = BoolSeq(len: len, data: newSeq[byte](byteCount(len)))

proc initBoolSeqOfCap*(cap: int = 4): BoolSeq {.inline.} =
  result = BoolSeq(len: 0, data: newSeqOfCap[byte](cap))

proc `[]`*(bs: BoolSeq, i: int): bool {.inline.} =
  when true:
    (bs.data[byteIndex(i)] and byteMask(i)) != 0
  else:
    let offset = byteOffset(i)
    bool((bs.data[byteIndex(i)] and (1.byte shl offset)) shr offset)

proc contains*(bs: BoolSeq, i: int): bool {.inline.} =
  bs[i]

proc incl*(bs: var BoolSeq, i: int) {.inline.} =
  bs.data[byteIndex(i)] = bs.data[byteIndex(i)] or byteMask(i)

proc excl*(bs: var BoolSeq, i: int) {.inline.} =
  bs.data[byteIndex(i)] = bs.data[byteIndex(i)] and not byteMask(i)

proc `[]=`*(bs: var BoolSeq, i: int, val: bool) {.inline.} =
  if val:
    incl(bs, i)
  else:
    excl(bs, i)

proc setLen*(bs: var BoolSeq, newLen: int) {.inline.} =
  bs.setLen(byteCount(newLen))
