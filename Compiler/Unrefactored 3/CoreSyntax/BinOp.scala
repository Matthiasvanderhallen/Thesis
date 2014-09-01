package CoreSyntax

sealed trait BinOp
case object Add extends BinOp
case object Sub extends BinOp
case object Mul extends BinOp
case object Rem extends BinOp