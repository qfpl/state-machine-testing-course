module Ed.Address where

data Direction
  = Forward
  | Backward

data Address
  = Period
  | Dollar
  | Nth Natural
  | Mark Char
  | Search Direction BRE
  | Step Direction Natural

data AddressOffset
  = N Direction Natural

data Addr = Addr Address [AddressOffset]
