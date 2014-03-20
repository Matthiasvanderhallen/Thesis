import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.RegexParsers

class Main {

}

sealed trait MLToken
case class Ident(value: String) extends MLToken
case class Number(value: Int) extends MLToken
//case object SignatureStart extends MLToken
//case object StructureStart extends MLToken
//case object end extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal
//case object TypeDef extends MLToken
//case object valDef extends MLToken
//case object Unknown extends MLToken

sealed trait MLTree
case class Type(name:Ident)
case class Declaration(ident:Ident, ascription: Type)
case class Definition(ident:Ident, ascription: Type, expression: MLTree)
case class Signature(value: List[Declaration])
case class Structure(ident:Ident, value: List[Definition])
//case class ValExpr(name:Ident, ascription: Type, expression: MLTree)

object Main extends App{
//  val identifier:Ident = new Ident("test")
//  val firstType:Type = new Type(identifier)
//  val decl:Declaration = new Declaration(new Ident("test"), firstType)
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

  println(MLLexer.apply(code))

}

object MLLexer extends RegexParsers {

  def number = """(\d+)""".r ^^ { x=>new Number(x.toInt) }
  def keyword = """\=|\(|\)|signature|sig|structure|struct|end|type|val|fun|\:\>|\:|->|\+|-|%""".r ^^ {new Keyword(_)}
  def identifier = """[a-zA-Z_][a-zA-Z0-9_\.]*""".r ^^ {new Ident(_)}
  def separator = """,""".r
  def literal = """\".*?\"""".r ^^ {new Literal(_)}

  def tokenizer = keyword | identifier | number | literal | separator

  def apply(input:String) = parseAll(rep(tokenizer), input)
}

object MLParser{

  def parseSignature(tokens:List)

  def apply(tokens:List[MLToken]) = x match {
    case Literal
  }
}