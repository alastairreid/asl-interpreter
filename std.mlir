// placeholder: not the real definition
!Std$RAM = i64 // note: this is an identifier for a RAM instance, not the storage itself
!Std$Integer = i64
!Std$String = i1

// !Std$Bits = !llvm.struct<(!Std$Integer, i128)> // pair of length and a bitvector

// !Std$Bits = i192 // top 64 bits are the width
//
// func.func private @Std$Bits$Constant(%size : !Std$Integer, %x : i128) -> !Std$Bits {
//   %0 = arith.extui %size : !Std$Integer to !Std$Bits
//   %1 = arith.extui %x : i128 to !Std$Bits
//   %2 = arith.constant 128 : !Std$Bits
//   %3 = arith.shli %0, %2 : !Std$Bits
//   %4 = arith.ori %3, %1 : !Std$Bits
//   func.return %4 : !Std$Bits
// }
//
// func.func private @Std$Bits$MyLength(%x : !Std$Bits) -> !Std$Integer {
//   %0 = arith.constant 128 : !Std$Bits
//   %1 = arith.shli %x, %0 : !Std$Bits
//   %2 = arith.trunci %1 : !Std$Bits to !Std$Integer
//   func.return %2 : !Std$Integer
// }
//
// func.func private @Std$Bits$Signed(%size : !Std$Integer, %x : !Std$Bits) -> !Std$Integer {
//   %0 = arith.trunci %x : !Std$Bits to !Std$Integer
//   func.return %0 : !Std$Integer
// }
//
// func.func private @Std$Bits$Unsigned(%size : !Std$Integer, %x : !Std$Bits) -> !Std$Integer {
//   %0 = arith.trunci %x : !Std$Bits to i63
//   %1 = arith.extui %0 : i63 to !Std$Integer
//   func.return %1 : !Std$Integer
// }
//
// func.func private @Std$Bits$Subtract(%size : !Std$Integer, %left : !Std$Bits, %right : !Std$Bits) -> !Std$Bits {
//   %0 = arith.trunci %left : !Std$Bits to i128
//   %1 = arith.trunci %right : !Std$Bits to i128
//   %2 = arith.subi %0, %1 : i128
//   %3 = arith.extui %2 : i128 to !Std$Bits
//   %minus_one = arith.constant -1 : !Std$Bits
//   %c64 = arith.constant 128 : !Std$Bits
//   %mask = arith.shli %minus_one, %c64 : !Std$Bits
//   %4 = arith.andi %left, %mask : !Std$Bits
//   %5 = arith.ori %4, %3 : !Std$Bits
//   func.return %5 : !Std$Bits
// }

!Std$Bits = i128 // todo: doesn't track length

func.func private @Std$Bits$Constant(%size : !Std$Integer, %x : i128) -> !Std$Bits {
  func.return %x : !Std$Bits
}

func.func private @Std$Bits$MyLength(%x : !Std$Bits) -> !Std$Integer

func.func private @Std$Bits$Signed(%size : !Std$Integer, %x : !Std$Bits) -> !Std$Integer {
  %0 = arith.trunci %x : !Std$Bits to !Std$Integer
  func.return %0 : !Std$Integer
}

func.func private @Std$Bits$Unsigned(%size : !Std$Integer, %x : !Std$Bits) -> !Std$Integer {
  %0 = arith.trunci %x : !Std$Bits to i63
  %1 = arith.extui %0 : i63 to !Std$Integer
  func.return %1 : !Std$Integer
}

func.func private @Std$Bits$Add(%size : !Std$Integer, %left : !Std$Bits, %right : !Std$Bits) -> !Std$Bits {
  %0 = arith.addi %left, %right : !Std$Bits
  func.return %0 : !Std$Bits
}

func.func private @Std$Bits$Subtract(%size : !Std$Integer, %left : !Std$Bits, %right : !Std$Bits) -> !Std$Bits {
  %0 = arith.subi %left, %right : !Std$Bits
  func.return %0 : !Std$Bits
}

func.func private @Std$Bits$Shift_Left_Logical_Restricted(%size : !Std$Integer, %x : !Std$Bits, %amount : !Std$Integer) -> !Std$Bits {
  %0 = arith.extui %amount : !Std$Integer to !Std$Bits
  %1 = arith.shli %x, %0 : !Std$Bits
  func.return %1 : !Std$Bits
}

func.func private @Std$Integer$Slice(%x : !Std$Integer, %ix : !Std$Integer, %size : !Std$Integer) -> !Std$Bits

func.func private @Std$Bits$Slice(%x : !Std$Bits, %ix : !Std$Integer, %size : !Std$Integer) -> !Std$Bits {
  %0 = arith.extsi %size : !Std$Integer to !Std$Bits
  %1 = arith.shrui %x, %0 : !Std$Bits
  %ones = arith.constant -1 : !Std$Bits
  %c128 = arith.constant 128 : !Std$Bits
  %amount = arith.subi %c128, %0 : !Std$Bits
  %mask = arith.shrui %ones, %amount : !Std$Bits
  %r = arith.andi %1, %mask : !Std$Bits
  func.return %r : !Std$Bits
}

func.func private @Std$Integer$Add(%left : !Std$Integer, %right : !Std$Integer) -> !Std$Integer {
  %r = arith.addi %left, %right : !Std$Integer
  func.return %r : !Std$Integer
}

func.func private @Std$Integer$Subtract(%left : !Std$Integer, %right : !Std$Integer) -> !Std$Integer {
  %r = arith.subi %left, %right : !Std$Integer
  func.return %r : !Std$Integer
}

func.func private @Std$Integer$Multiply(%left : !Std$Integer, %right : !Std$Integer) -> !Std$Integer {
  %r = arith.muli %left, %right : !Std$Integer
  func.return %r : !Std$Integer
}

func.func private @Std$Integer$Negate(%a : !Std$Integer) -> !Std$Integer {
  %zero = arith.constant 0 : !Std$Integer
  %r = arith.subi %zero, %a : !Std$Integer
  func.return %r : !Std$Integer
}

func.func private @Std$Integer$Power2(%exp : !Std$Integer) -> !Std$Integer {
  %one = arith.constant 1 : !Std$Integer
  %r = arith.shli %one, %exp : !Std$Integer
  func.return %r : !Std$Integer
}

func.func private @Std$Integer$Eq(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi eq, %left, %right : !Std$Integer
  func.return %r : i1
}

func.func private @Std$Integer$Ne(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi ne, %left, %right : !Std$Integer
  func.return %r : i1
}

func.func private @Std$Integer$Le(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi sle, %left, %right : !Std$Integer
  func.return %r : i1
}

func.func private @Std$Integer$Lt(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi slt, %left, %right : !Std$Integer
  func.return %r : i1
}

func.func private @Std$Integer$Gt(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi sgt, %left, %right : !Std$Integer
  func.return %r : i1
}

func.func private @Std$Integer$Ge(%left : !Std$Integer, %right : !Std$Integer) -> i1 {
  %r = arith.cmpi sge, %left, %right : !Std$Integer
  func.return %r : i1
}

!Std$Bit = !Std$Bits
