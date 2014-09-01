import CoreSyntax._
import ModuleSyntax._
import ProgramSyntax._
import TypeSystem._
import Compiler._

object Programs {
  val vara = new VarType(new Ident("a"))
  val paira = new StructType(new Ident("pair"), 1, List(vara))
  val cred = new StructType(new Ident("cred"), 0, List())

  val symmetriccipher = new Signature(new Ident("SYMMETRICCIPHER"),
    List(
      new OpaqueTypeDeclaration(new Ident("cred"), 0),
      new ValDeclaration(new Ident("newcredentials"), new StructType(new Ident("cred"),0, List())),
      new ValDeclaration(new Ident("encrypt"), new FuncType(List(TypeSystem.Integer, cred), TypeSystem.Integer)),
      new ValDeclaration(new Ident("decrypt"), new FuncType(List(TypeSystem.Integer, cred), TypeSystem.Integer))//,
      //new ModuleSyntax.ValDeclaration(new Ident("merge"), new TypeSystem.FuncType(List(paira, paira), paira))
    )
  )

  val caesar = new  Structure(new Ident("Caesar"), new Ident("SYMMETRICCIPHER"),
    List(
      new TypeDefinition(new Ident("cred"), 0, TypeSystem.Integer),
      new ValDefinition(new Ident("newcredentials"), cred, new ValExpr(new Ident("rand"))),
      new FunDefinition(new Ident("encrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(TypeSystem.Integer, cred), TypeSystem.Integer), new BinOpExpr(Rem, new BinOpExpr(Add, new ValExpr(new Ident("a")), new ValExpr(new Ident("cred"))), ConstExpr(26))),
      new FunDefinition(new Ident("decrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(TypeSystem.Integer, cred), TypeSystem.Integer), new BinOpExpr(Rem, new BinOpExpr(Sub, new ValExpr(new Ident("a")), new ValExpr(new Ident("cred"))), ConstExpr(26))),
      //new ModuleSyntax.FunDefinition(new Ident("merge"), List(new Ident("left"), new Ident("right")), new TypeSystem.FuncType(List(paira, paira), paira), CoreSyntax.ConstExpr(3)),
      new ValDefinition(new Ident("seed"), TypeSystem.Integer, new ConsExpr(ConstExpr(5), new ConsExpr(ConstExpr(3),new Empty(TypeSystem.Integer)))),//CoreSyntax.ConstExpr(3)),
      new ValDefinition(new Ident("rand"), TypeSystem.Integer, ValExpr(new Ident("seed")))
    )
  )

  val pairsig = new Signature(new Ident("PAIRSIG"),
    List(
      new OpaqueTypeDeclaration(new Ident("pair"), 1),
      new ValDeclaration(new Ident("createPair"), new FuncType(List(vara,vara), new StructType(new Ident("pair"),1, List(vara)))),
      new ValDeclaration(new Ident("getLeft"), new FuncType(List(paira), vara)),
      new ValDeclaration(new Ident("getRight"), new FuncType(List(paira), vara))
    ))

  val pair = new Structure(new Ident("Pair"), new Ident("PAIRSIG"),
    List(
      new TypeDefinition(new Ident("pair"), 1, new PairType(vara, vara)),
      new FunDefinition(new Ident("createPair"), List(new Ident("left"), new Ident("right")), new FuncType(List(vara,vara), paira), PairExpr(ValExpr(new Ident("left")),ValExpr(new Ident("right")))),
      new FunDefinition(new Ident("getLeft"), List(new Ident("pair")), new FuncType(List(paira), vara), PairAccess(ValExpr(new Ident("pair")), vara, Left)),
      new FunDefinition(new Ident("getRight"), List(new Ident("pair")), new FuncType(List(paira), vara), PairAccess(ValExpr(new Ident("pair")), vara, Right))// CoreSyntax.ConstExpr(3)),
    )
  )

  val program1 = new Program(List(symmetriccipher, pairsig), List(caesar, pair))
}
