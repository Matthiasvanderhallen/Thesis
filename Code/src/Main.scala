import scala.util.parsing.combinator.RegexParsers

sealed trait MLToken
case class Ident(value: String) extends MLToken
case class Number(value: Int) extends MLToken
case object Delimiter extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal


case class Program(val signatures:List[Signature], val structures:List[Structure], val tle:Expr)

sealed trait Type
case object Integer extends Type
case class FuncType(left:Type, right:Type) extends Type
case class VarType(ident:Ident) extends Type

case class Structure(ident:Ident, signature:Signature, value: List[Definition])
sealed trait Definition
case class ValDefinition(ident:Ident, ascription: TypeDefinition, expression: Expr) extends Definition
case class TypeDefinition (name:Ident, definition: Type) extends Definition

case class Signature(ident:Ident, value: List[Declaration])
sealed trait Declaration
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration
case class TypeDeclaration(ident:Ident, definition: Type) extends Declaration



sealed trait Expr
case class ValExpr(name:Ident) extends Expr
case class ConstExpr(value: Number) extends Expr
case class CallExpr(name:Ident, args: List[Expr])
case class BinOpExpr(name:String, left:Expr, right:Expr)

object Main extends App{
  val code = "signature SYMMETRICCIPHER = sig \n" +
    "type cred \n" +
    "val newcredentials : cred \n" +
    "val encrypt : int -> cred -> int \n" +
    "val decrypt : int -> cred -> int \n" +
    "end \n" +
    "structure Caesar :> SYMMETRICCIPHER = struct type cred = int\n" +
    "fun newcredentials = rand\n" +
    "fun encrypt(a,cred) = (a + cred)%26 \n" +
    "fun decrypt (a, cred) = (a - cred)%26\n" +
    "val seed = 3\n"
    "fun rand = time.now * seed\n" +
    "end"


  val tokenList:List[MLToken] = MLLexer.apply(code)
  println(tokenList)
  println(MLParser.apply(tokenList, new Program(List(), List(), null)))
  println(MLParser.parseDeclaration(List(Keyword("val"), Ident("newcredentials"), Keyword(":"), Ident("cred"))))
  println(MLParser.parseDeclaration(List(Keyword("val"), Ident("encrypt"), Keyword(":"), Ident("int"), Keyword("->"), Ident("cred"), Keyword("->"), Ident("int"))))
}

object MLParser{

  def parseSignature(tokens:List[MLToken]):Signature = tokens match {
    case ((id:Ident)::Keyword("=")::Keyword("sig")::tail) => {
      Signature(id, List())
    }
    case _ => sys.error("Invalid Signature definition")
  }

  def parseDeclarations(tokens:List[MLToken]) = {
    val declarations =  tokens.takeWhile(_ != Keyword("end"))
  }

  def parseDeclaration(tokens:List[MLToken]):Declaration = tokens match {
    case (Keyword("type")::(id@Ident(_))::Keyword("=")::(id2@Ident(_))::tail) => TypeDeclaration(id, if(id2 == Ident("int")) Integer else VarType(id2))
    case (Keyword("type")::(id@Ident(_))::tail) => TypeDeclaration(id, VarType(id))
    case (Keyword("val")::(id@Ident(_))::Keyword(":")::tail) => {
      var types = tail.takeWhile(x => (x match {
        case Ident(_) => true
        case Keyword("->") => true
          //TODO: add braces support
        case _ => false
      }))

      var valtype = if(types.length > 1){
        types = types.reverse
        var firstElem:VarType = types.head match{
          case id@Ident(_) => VarType(id:Ident)
          case _ => sys.error("unparsable type")
        }
        types.drop(1).foldRight(firstElem:Type)((elem,acc) => elem match {
          case id@Ident(_) => FuncType(VarType(id), acc)
          case Keyword("->") => acc
          case _ => sys.error("unparsable type")
        })
      }
      else{
        types(0) match {
          case id@Ident(_) => VarType(id)
          case _ => sys.error("Non type provided as type")
        }
      }

      ValDeclaration(id, valtype)
    }
  }

  def apply(tokens:List[MLToken], program:Program):Program = tokens match {
    case ((x:Literal)::tail) => sys.error("No naked string literal on top level allowed")
    case ((x@Keyword("signature"))::tail) => Program(List(parseSignature(tail)), List(), null)
    case _ => new Program(List(), List(), null)
  }
}

object MLLexer extends RegexParsers {

  def number = """(\d+)""".r ^^ { x=>Number(x.toInt) }
  def keyword = """\=|\(|\)|signature|sig|structure|struct|end|type|val|fun|\:\>|\:|->|\+|-|%""".r ^^ {new Keyword(_)}
  def identifier = """[a-zA-Z_][a-zA-Z0-9_\.]*""".r ^^ {Ident(_)}
  def separator:Parser[MLToken] = """,""".r ^^^ {Delimiter}
  def literal:Parser[MLToken] = """\".*?\"""".r ^^ {Literal(_)}

  def tokenizer = keyword | identifier | number | literal | separator

  def apply(input:String) = parseAll(rep(tokenizer), input) match {
    case Success(result,_) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

