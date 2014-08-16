import scala.util.parsing.combinator.RegexParsers

sealed trait MLToken
case class Ident(value: String) extends MLToken {
  override def toString() = value;
}
case class Number(value: Int) extends MLToken
case object Delimiter extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal

case class Program(val signatures:List[Signature], val structures:List[Structure])

case class Translation(var types:String, var entryPoints:String, var values:String, var opaqueTypes:Int, var mapping:List[(Int,Int)])

sealed trait Type{
  def intRepresentation:Int
}

case object Integer extends Type {
  override def toString() = "%int"
  def intRepresentation = 1;
}

case class FuncType(left:List[Type], right:Type) extends Type{
  def intRepresentation = 5;
}

case class VarType(ident:Ident) extends Type {
  override def toString() = "%tyvar"
  def intRepresentation = 2;
}

case class StructType(typeId:Ident) extends Type {
  def intRepresentation = -1; //Hmm
}

case class PairType(left:Type, right:Type) extends Type {
  override def toString() = "%pair"
  def intRepresentation = 3;
}

case class ListType(inner:Type) extends Type {
  override def toString() = "%array"
  def intRepresentation = 4;
}


case class Structure(ident:Ident, signature:Ident, value: List[Definition])

sealed trait Definition
case class ValDefinition(ident:Ident, ascription: Type, expression: Expr) extends Definition
case class FunDefinition(ident:Ident, variables: List[Ident], ascription: FuncType, expression: Expr) extends Definition
case class TypeDefinition (ident:Ident, definition: Type) extends Definition

case class Signature(ident:Ident, value: List[Declaration])
sealed trait Declaration
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration
sealed trait TypeDeclaration extends Declaration
case class OpaqueTypeDeclaration(ident:Ident) extends TypeDeclaration
case class TransparentTypeDeclaration(ident:Ident, definition: Type) extends TypeDeclaration



sealed trait Expr
case class ValExpr(ident:Ident) extends Expr
case class ConstExpr(value: Int) extends Expr
case class CallExpr(name:Ident, args: List[Expr])
case class BinOpExpr(op:BinOp, left:Expr, right:Expr)

sealed trait BinOp
case object Add extends BinOp
case object Sub extends BinOp
case object Mul extends BinOp
case object Rem extends BinOp

object Main extends App{
  val code = "signature SYMMETRICCIPHER = sig \n" +
    "type cred \n" +
    "val newcredentials : cred \n" +
    "val encrypt : int -> cred -> int \n" +
    "val decrypt : int -> cred -> int \n" +
    "end \n" +
    "structure Caesar :> SYMMETRICCIPHER = struct type cred = int\n" +
    "fun newcredentials = rand\n" +
    "fun encrypt a, cred = (a + cred)%26 \n" +
    "fun decrypt a, cred = (a - cred)%26\n" +
    "val seed = 3\n" +
    "fun rand = time.now * seed\n" +
    "end"

  val symmetriccipher = new Signature(new Ident("SYMMETRICCIPHER"),
                                 List(
                                      new OpaqueTypeDeclaration(new Ident("cred")),
                                      new ValDeclaration(new Ident("newcredentials"), new StructType(new Ident("cred"))),
                                      new ValDeclaration(new Ident("encrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"))), Integer)),
                                      new ValDeclaration(new Ident("decrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"))), Integer))
                                 )
                                 )

  val caesar = new  Structure(new Ident("Caesar"), new Ident("SYMMETRICCIPHER"),
                      List(
                        new TypeDefinition(new Ident("cred"), Integer),
                        new ValDefinition(new Ident("newcredentials"), new StructType(new Ident("cred")), ConstExpr(3)),
                        new FunDefinition(new Ident("encrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"))), Integer), ConstExpr(3)),
                        new FunDefinition(new Ident("decrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"))), Integer), ConstExpr(3)),
                        new ValDefinition(new Ident("seed"), Integer, ConstExpr(3)),
                        new ValDefinition(new Ident("rand"), Integer, ConstExpr(3))
                      )
                    )

  val caesar2 = new Structure(new Ident("ACaesar"), new Ident("SYMMETRICCIPHER"), List())

  val program = new Program(List(symmetriccipher), List(caesar, caesar2))

  println(translate(program))

  var entrypoints:String = ""

  def translate(program:Program):Translation={
    var translation = new Translation("", "", "", 4, List());

    val structures = program.structures.sortBy(struct => struct.ident.value)

    for (structure <- program.structures){
      val sig = program.signatures.find(sig => sig.ident == structure.signature).get
      translation = translateStructure(translation, structure, sig);
    }

    return translation;
  }

  def translateStructure(trans:Translation, structure:Structure, signature:Signature):Translation= {

    var translation = trans;

    val types = structure.value.filter{case TypeDefinition(_,_) => true; case _ => false}
    val values = structure.value
                 .filter{case TypeDefinition(_,_) => false; case _ => true}
                 .sortBy{case TypeDefinition(ident,_) => ident.value; case ValDefinition(ident,_,_) => ident.value; case FunDefinition(ident,_,_,_) => ident.value}

    for(typedef <- types){
      translation = translateTypes(translation, typedef, structure, signature)
    }

    println(types)
    println(values)

    return translation;
  }

  def translateTypes(trans:Translation, definition:Definition, structure:Structure, signature:Signature):Translation = {
    var translation = trans;

    if(true){} //TODO: check if opaque type

    val typedef = definition match{
      case x:TypeDefinition => x
      case _ => {
        throw new RuntimeException("Typedefinition expected, but other definition received");
      }
    }

//    val implementation = typedef.definition match{
//      case Integer => "%int"
//      case PairType(_,_) => "%pair"
//      case ListType(_) => "%array"
//      case _ => ""
//    }

    translation.opaqueTypes = translation.opaqueTypes + 1;
    translation.types = translation.types.concat("%" + structure.ident + "." + typedef.ident + " = type {" + typedef.definition + "}; " + translation.opaqueTypes + "\n")
    translation.mapping = (translation.opaqueTypes, typedef.definition.intRepresentation) :: translation.mapping;


    return translation;
  }

  //def translateTypes
//    trans match{
//      case Program(signatures, structures)=>
//          list.fold()
//      case Structure() => return "2"
//      case Expr => return "3"
//    }

}


