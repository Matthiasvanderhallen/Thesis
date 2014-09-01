sealed trait Expr
case class ValExpr(ident:Ident) extends Expr
case class ConstExpr(value: Int) extends Expr
case class CallExpr(name:Ident, args: List[Expr]) extends Expr
case class BinOpExpr(op:BinOp, left:Expr, right:Expr) extends Expr
case class LetExpr(ident:Ident, bind:Expr, in:Expr) extends Expr
case class PairExpr(left:Expr, right:Expr) extends Expr
//case class LeftExpr(pair:Expr, annot:Type) extends Expr
//case class RightExpr(pair:Expr, annot:Type) extends Expr
case class PairAccess(pair:Expr, annot:Type, side:Side) extends Expr
//sealed trait ListExpr extends Expr
case class Empty(annot:Type) extends Expr
case class ConsExpr(elem:Expr, tail:Expr) extends Expr
case class ListIndex(list:Expr, index:Int, annot:Type) extends Expr

sealed trait Side{
  def loc:Int
}
case object Left extends Side{
  def loc = 0;
}
case object Right extends Side{
  def loc = 1;
}

sealed trait BinOp
case object Add extends BinOp
case object Sub extends BinOp
case object Mul extends BinOp
case object Rem extends BinOp