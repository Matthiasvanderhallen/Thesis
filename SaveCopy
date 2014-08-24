import com.sun.tools.javac.code.Type.TypeVar
import scala.util.parsing.combinator.RegexParsers

sealed trait MLToken
case class Ident(value: String) extends MLToken {
  override def toString() = value;
  def ord():Int = {
    return value.charAt(0).toInt-97;
  }

  def internalize(structure:Structure):String = {
    if(value.contains("."))
      return value;
    else
      return s"${structure.ident.value}.${value}"
  }
}
case class Number(value: Int) extends MLToken
case object Delimiter extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal

case class Program(val signatures:List[Signature], val structures:List[Structure])

case class Translation(program:Program, var types:String, var entryPoints:String, var values:String, var firstPass:Boolean, var opaqueTypes:Int, var opaqueTypeMapping:Map[String,Int], var implMapping:List[(Int,Int)], var tyvarMapping:Map[Int,Type]) {
  override def toString() = {
    val defaultTypes =  "%int = type i64 ; 1\n" +
                        "%tyvar = type {%int, %int} ; 2\n" +
                        "%pair = type {%tyvar*, %tyvar*} ; 3\n" +
                        "%array = type {%int, [0 x %tyvar*]} ; 4\n"

    val declarations =  "declare i8* @malloc(%int)\n" +
                        "declare void @free(i8*)\n" +
                        "declare void @exit(i32)\n" +
                        "declare %int @mask(%int, %int)\n" +
                        "declare %int @unmask(%int)\n" +
                        "declare %int @unmasktype(%int)\n" +
                        "declare i1 \t @tyvarcheck(%tyvar*, %tyvar*)\n"

    val main =  "define %int @main(){\n" +
                "\tret %int 0\n" +
                "}\n"
    defaultTypes +";Non-standard Types\n" +types + "\n"+ declarations + "\n" + entryPoints + "\n" + values + main
  }

  def getImplTypeInt(search:Int):Int = {
    val result = implMapping.filter{case (search, _) => true; case _ => false}.head._2
    if(result > 5)
      return getImplTypeInt(result)
    else
      return result
  }

  def getOpaqueType(str:String):Int = {
    return opaqueTypeMapping.get(str).get
  }

  def getImplType(search:Int):Type = {
    return tyvarMapping.get(search).get
  }

  def getFuncType(structure:Structure, search:Ident):FuncType = {
    if(search.value.contains(".")){
      val defStructIdent = new Ident(search.value.split("""\.""")(0))
      val valueIdent = new Ident(search.value.split("""\.""")(1))
      if(defStructIdent == structure.ident)
        return structure.value.find{case FunDefinition(`search`,_,_,_) => true ;case _ => false}.get.asInstanceOf[FunDefinition].ascription

      val defStruct = program.structures.find{case Structure(`defStructIdent`,_,_) => true ; case _ => false}.get
      val sigId = defStruct.signature
      val defSignature = program.signatures.find{case Signature(`sigId`,_) => true; case _ => false}.get
      return defSignature.value.find{case ValDeclaration(`valueIdent`,_) => true; case _ => false}.get.asInstanceOf[ValDeclaration].ascription.asInstanceOf[FuncType]
    }else{
      return structure.value.find{case FunDefinition(`search`,_,_,_) => true ;case _ => false}.get.asInstanceOf[FunDefinition].ascription
    }
  }

  def getValType(struct:Structure, search:Ident) : Type = {
    if(search.value.contains(".")){
      val defStructIdent = new Ident(search.value.split("""\.""")(0))
      val valueIdent = new Ident(search.value.split("""\.""")(1))
      if(defStructIdent == struct.ident)
        return struct.value.find{case ValDefinition(`search`,_,_) => true ; case _ => false}.get.asInstanceOf[ValDefinition].ascription

      val defStruct = program.structures.find{case Structure(`defStructIdent`,_,_) => true; case _ => false}.get
      val sigId = defStruct.signature
      val defSignature = program.signatures.find{case Signature(`sigId`,_) => true; case _ => false}.get
      return defSignature.value.find{case ValDeclaration(`valueIdent`,_) => true; case _ => false}.get.asInstanceOf[ValDeclaration].ascription
    }else{
      return struct.value.find{case ValDefinition(`search`,_,_) => true ; case _ => false}.get.asInstanceOf[ValDefinition].ascription
    }
  }
}

sealed trait Type{
  def intRepresentation:Int
  def getVarTypes(trans:Translation):List[Int]
  def qualifiedName(structure:Structure):String //Structname.typename, array, pair, int
  def getTypeString(structure:Structure):String //%Structname.typename*, %array*, %pair*, %int
}

case object Integer extends Type {
  override def toString() = "%int"
  def intRepresentation = 1;
  def getVarTypes(trans:Translation):List[Int] = {
    return List()
  }
  def qualifiedName(structure:Structure):String = {
    return "int"
  }
  
  def getTypeString(structure:Structure):String = {
    return toString() //No pointers to ints!
  }
}

case class FuncType(left:List[Type], right:Type) extends Type{
  def intRepresentation = 5;
  def getVarTypes(trans:Translation):List[Int] = {
    throw new UnsupportedOperationException();
  }
  def qualifiedName(structure:Structure):String = {
    throw new UnsupportedOperationException("Can't internalize a functype");
  }
  def getTypeString(structure:Structure):String = {
    throw new UnsupportedOperationException("Functions are not a first class value, and are not passable")
  }
}

case class VarType(ident:Ident) extends Type {
  override def toString() = "%tyvar"
  def intRepresentation = 2;

  def getVarTypes(trans:Translation):List[Int] = {
    return List(ident.value.charAt(0).toInt-97)
  }

  def qualifiedName(structure:Structure):String = {
    throw new UnsupportedOperationException("Can't internalize a VarType");
  }

  def getTypeString(structure:Structure):String = {
    return "%tyvar*"
  }
}

case class StructType(typeId:Ident, nrTyvars:Int, tyvars:List[Type]) extends Type {
  def intRepresentation = -1; //Hmm

  def qualifiedName(structure:Structure):String = {
    if(typeId.value.contains("."))
      return s"${typeId.value}"
    else
      return s"${structure.ident.value}.${typeId.value}"
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
  }

  def getVarTypes(trans:Translation):List[Int] = {
    return trans.getImplType(trans.getOpaqueType(typeId.value)).getVarTypes(trans)
  }

  override def toString() = typeId.value

}

case class PairType(left:Type, right:Type) extends Type {
  override def toString() = "%pair"
  def intRepresentation = 3;
  def getVarTypes(trans:Translation):List[Int] = {
    return left.getVarTypes(trans)++right.getVarTypes(trans)
  }
  def qualifiedName(structure:Structure):String = {
    return "pair"; //Mogelijkerwijs + *
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
  }
}

case class ListType(inner:Type) extends Type {
  override def toString() = "%array"
  def intRepresentation = 4;

  def getVarTypes(trans:Translation):List[Int] = {
    return inner.getVarTypes(trans)
  }

  def qualifiedName(structure:Structure):String = {
    return "array"; //Mogelijkerwijs + *
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
  }
}


case class Structure(ident:Ident, signature:Ident, value: List[Definition])

sealed trait Definition
case class ValDefinition(ident:Ident, ascription: Type, expression: Expr) extends Definition
case class FunDefinition(ident:Ident, variables:List[Ident], ascription: FuncType, expression: Expr) extends Definition
case class TypeDefinition (ident:Ident, tyvars:Int, definition: Type) extends Definition

case class Signature(ident:Ident, value: List[Declaration])
sealed trait Declaration
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration
sealed trait TypeDeclaration extends Declaration
case class OpaqueTypeDeclaration(ident:Ident, tyvars:Int) extends TypeDeclaration
case class TransparentTypeDeclaration(ident:Ident, tyvars:Int, definition: Type) extends TypeDeclaration



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
                                      new OpaqueTypeDeclaration(new Ident("cred"), 0),
                                      //new OpaqueTypeDeclaration(new Ident("pair"), 1),
                                      new ValDeclaration(new Ident("newcredentials"), new StructType(new Ident("cred"),0, List())),
                                      new ValDeclaration(new Ident("encrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"),0, List())), Integer)),
                                      new ValDeclaration(new Ident("decrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"),0, List())), Integer))//,
                                      //new ValDeclaration(new Ident("createPair"), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"),1, List(new VarType(new Ident("a")))))),
                                      //new ValDeclaration(new Ident("getLeft"), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a")))),
                                      //new ValDeclaration(new Ident("merge"), new FuncType(List(paira, paira), paira))
                                 )
                                 )

  val paira = new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))
  val caesar = new  Structure(new Ident("Caesar"), new Ident("SYMMETRICCIPHER"),
                      List(
                        new TypeDefinition(new Ident("cred"), 0, Integer),
                        //new TypeDefinition(new Ident("pair"), 1, PairType(new VarType(new Ident("a")), new VarType(new Ident("a")))),
                        new ValDefinition(new Ident("newcredentials"), new StructType(new Ident("cred"), 0, List()), new ValExpr(new Ident("rand"))),
                        new FunDefinition(new Ident("encrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"), 0, List())), Integer), new BinOpExpr(Rem, new BinOpExpr(Add, new ValExpr(new Ident("a")), new ValExpr(new Ident("cred"))), ConstExpr(26))),
                        new FunDefinition(new Ident("decrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"), 0, List())), Integer), new BinOpExpr(Rem, new BinOpExpr(Sub, new ValExpr(new Ident("a")), new ValExpr(new Ident("cred"))), ConstExpr(26))),
                        //new FunDefinition(new Ident("createPair"), List(new Ident("left"), new Ident("right")), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), PairExpr(ValExpr(new Ident("left")),ValExpr(new Ident("right")))),
                        //new FunDefinition(new Ident("getLeft"), List(new Ident("pair")), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))), CallExpr(new Ident("createPair"),List(ConstExpr(3)))), // ConstExpr(3)),
                        //new FunDefinition(new Ident("merge"), List(new Ident("left"), new Ident("right")), new FuncType(List(paira, paira), paira), ConstExpr(3)),
                        new ValDefinition(new Ident("seed"), Integer, new ConsExpr(ConstExpr(5), new ConsExpr(ConstExpr(3),new Empty(Integer)))),//ConstExpr(3)),
                        new ValDefinition(new Ident("rand"), Integer, ValExpr(new Ident("seed")))
                      )
                    )

  val pairsig = new Signature(new Ident("PAIRSIG"),
                              List(
                                new OpaqueTypeDeclaration(new Ident("pair"), 1),
                                new ValDeclaration(new Ident("createPair"), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"),1, List(new VarType(new Ident("a")))))),
                                new ValDeclaration(new Ident("getLeft"), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a")))),
                                new ValDeclaration(new Ident("getRight"), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))))
                              ))

  val pair = new Structure(new Ident("Pair"), new Ident("PAIRSIG"),
                            List(
                            new TypeDefinition(new Ident("pair"), 1, new PairType(new VarType(new Ident("a")), new VarType(new Ident("a")))),
                            new FunDefinition(new Ident("createPair"), List(new Ident("left"), new Ident("right")), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), PairExpr(ValExpr(new Ident("left")),ValExpr(new Ident("right")))),
                            new FunDefinition(new Ident("getLeft"), List(new Ident("pair")), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))), PairAccess(ValExpr(new Ident("pair")), new VarType(new Ident("a")), Left)),
                            new FunDefinition(new Ident("getRight"), List(new Ident("pair")), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))), PairAccess(ValExpr(new Ident("pair")), new VarType(new Ident("a")), Right))// ConstExpr(3)),
                            )
                          )

  val caesar2 = new Structure(new Ident("ACaesar"), new Ident("SYMMETRICCIPHER"), List())

  val program = new Program(List(symmetriccipher, pairsig), List(caesar, caesar2, pair))

  println(translate(program))

  var entrypoints:String = ""

  def translate(program:Program):Translation={
    var translation = new Translation(program, "", "", "",true, 5, Map(), List(), Map());

    val structures = program.structures.sortBy(struct => struct.ident.value)

    for (structure <- program.structures){
      val sig = program.signatures.find(sig => sig.ident == structure.signature).get
      translation = translateStructure(translation, structure, sig);
    }

    translation.firstPass = false;

    for (structure <- program.structures){
      val sig = program.signatures.find(sig => sig.ident == structure.signature).get
      translation = translateStructure(translation, structure, sig);
    }

    return translation;
  }

  def translateStructure(trans:Translation, structure:Structure, signature:Signature):Translation= {

    var translation = trans;

    val types = structure.value.filter{case TypeDefinition(_,_,_) => true; case _ => false}
    val values = structure.value
                 .filter{case TypeDefinition(_,_,_) => false; case _ => true}
                 .sortBy{case TypeDefinition(ident,_,_) => ident.value; case ValDefinition(ident,_,_) => ident.value; case FunDefinition(ident,_,_,_) => ident.value}
    if(translation.firstPass){
      for(typedef <- types){
        translation = translateDefinition(translation, typedef, structure, signature)
      }
    } else {
      for(valdef <- values){
        translation = translateDefinition(translation, valdef, structure, signature)
      }
    }

    return translation;
  }

  def translateDefinition(trans:Translation, definition:Definition, structure:Structure, signature:Signature):Translation = {
    val translation = definition match{
      case x:TypeDefinition => translateType(trans, x, structure, signature)
      case x:ValDefinition => translateValue(trans, x, structure, signature)
      case x:FunDefinition => translateFun(trans, x, structure, signature)
    }

    return translation
  }

  def translateType(trans:Translation, typedef:TypeDefinition, structure:Structure, signature:Signature):Translation = {
    var translation = trans;

    val typdefident = typedef.ident

    val declaration = signature.value.find{case OpaqueTypeDeclaration(`typdefident`,_)=>true; case _ => false}

    if(declaration.isEmpty){
      //Transparant type
      translation.types = translation.types.concat("%" + structure.ident + "." + typedef.ident + " = type {" + typedef.definition + "}; " + typedef.definition.intRepresentation + "\n")
    }else{
      //Opaque type
      translation.opaqueTypes = translation.opaqueTypes + 1;
      translation.types = translation.types.concat("%" + structure.ident + "." + typedef.ident + " = type {" + typedef.definition + "}; " + translation.opaqueTypes + "\n") //Todo wat met typedef dependencies
      translation.opaqueTypeMapping = translation.opaqueTypeMapping + ((structure.ident.value + "." + typedef.ident.value) -> translation.opaqueTypes)
      translation.implMapping = (translation.opaqueTypes, typedef.definition.intRepresentation) :: translation.implMapping;
      translation.tyvarMapping = translation.tyvarMapping + (translation.opaqueTypes -> typedef.definition);
    }

    return translation;
  }

  def translateFun(trans:Translation, fundef:FunDefinition, structure:Structure, signature:Signature):Translation = {
    var translation = trans

    val declaration = signature.value.exists{case ValDeclaration(fundef.ident,_)=>true; case _ =>false}

    val ident = structure.ident.value + "." + fundef.ident.value

    val retInt = fundef.ascription.right == Integer;
    val retTyvar = fundef.ascription.right.isInstanceOf[VarType]
    val retTypeId = fundef.ascription.right.getTypeString(structure)
    val retTypeInt = fundef.ascription.right match{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.qualifiedName(structure))
      case Integer => Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => fundef.ascription.right.intRepresentation
    }

    val argInt = fundef.ascription.left.map(x => x == Integer)
    val argTyvar = fundef.ascription.left.map(x => x.isInstanceOf[VarType])
    val argTypeId = fundef.ascription.left.map{x => x.getTypeString(structure)}

    val argTypeInt = fundef.ascription.left.map{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.qualifiedName(structure))
      case Integer => Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => fundef.ascription.right.intRepresentation
    }

    val argNames = fundef.variables.map{x=>"%"+x.value}
    val argForInternal = argTypeId.zip(argNames).map{x => x._1 + " "+x._2}.mkString(", ")

    //Internal function output
    var value = s"define private $retTypeId @$ident"+s"_internal($argForInternal){\n"
    value += translateBody(trans, fundef, structure, signature, fundef.ascription.left.zip(argNames), retTypeId)
    value +="}\n\n"

    //External function output
    if(declaration){
      val argumentsAsInt = argTyvar.zip(argNames).map{x => "%int "+x._2+".in" + (if(x._1){", i2 " + x._2 + ".isMask"}else{""})}.mkString(", ")

      value += s"define %int @${ident}_stub($argumentsAsInt) noinline {\n"
      value += s"\t;Switch stack, move parameters\n"

      val argInformation = (fundef.variables, 1 to fundef.variables.length,(argTyvar,argTypeId,argTypeInt).zipped.toList).zipped.toList
      val argProcessingCode = argInformation.map{
        case (name,nr,(false,typeId,1)) => s"\tProcess$nr:\n" +
                                           s"\t\t%$name = add %int %$name.in, 0 ; Renaming trick \n" +
                                           s"\t\tbr label %Process${nr+1}\n\n"
        case (name,nr,(false,typeId,typeInt)) => s"\tProcess$nr:\n" +
                                                 s"\t\t%$name.addr = call %int @unmask(%int %$name.in)\n" +
                                                 s"\t\t%$name.type = call %int @unmasktype(%int %$name.in)\n" +
                                                 s"\t\t%$name = inttoptr %int %$name.addr to $typeId\n" +
                                                 s"\t\t%$name.check = icmp ne %int %$name.type, $typeInt\n" +
                                                 s"\t\tbr i1 %$name.check, label %Error, label %Process${nr+1}\n\n"
        case (name,nr,(true,typeId, typeInt)) => s"\tProcess$nr:\n" +
                                                 s"\t\t%$name.tyvar.addr = call i8* @malloc(%int 16)\n" +
                                                 s"\t\t%$name = bitcast i8* %$name.tyvar.addr to %tyvar*\n" +
                                                 s"\t\tswitch i2 %$name.isMask, label %Unmask$nr [i2 0, label %External$nr\n" +
                                                 s"\t\t${" " * (s"switch i2 %$name.isMask, label %Unmask$nr [").length}i2 1, label %Int$nr]\n\n" +
                                                 s"\tUnmask$nr:\n" +
                                                 s"\t\t%$name.addr = call %int @unmask(%int %$name.in)\n" +
                                                 s"\t\t%$name.type = call %int @unmasktype(%int %$name.in)\n" +
                                                 s"\t\t%$name.unmask.1 = insertvalue %tyvar undef, %int %$name.addr, 0\n" +
                                                 s"\t\t%$name.unmask.2 = insertvalue %tyvar %$name.unmask.1, %int %$name.type, 1\n" +
                                                 s"\t\tbr label %Create$nr\n\n" +
                                                 s"\tExternal$nr:\n" +
                                                 s"\t\t%$name.ext.1 = insertvalue %tyvar undef, %int %$name.in, 0\n" +
                                                 s"\t\t%$name.ext.2 = insertvalue %tyvar %$name.ext.1, %int 0, 1\n" +
                                                 s"\t\tbr label %Create$nr\n\n" +
                                                 s"\t\tInt$nr:\n" +
                                                 s"\t\t%$name.int.1 = insertvalue %tyvar undef, %int %$name.in, 0\n" +
                                                 s"\t\t%$name.int.2 = insertvalue %tyvar %$name.int.1, %int 1, 1\n" +
                                                 s"\t\tbr label %Create$nr\n\n" +
                                                 s"\tCreate$nr:\n" +
                                                 s"\t\t%$name.tyvar = phi %tyvar [%$name.unmask.2, %Unmask$nr], [%$name.ext.2, %External$nr], [%$name.int.2, %Int$nr]\n" +
                                                 s"\t\tstore %tyvar %$name.tyvar, %tyvar* %$name\n"+
                                                 s"\t\tbr label %Process${nr+1}\n\n"
      }


      value += argProcessingCode.mkString("")+ "\tProcess"+ (fundef.variables.length+1)+":\n"

      value += accessTyvars(fundef.ascription.left, fundef.variables,trans,structure,"")

      if(!retInt){
        value += s"\t%ret.ptr = call $retTypeId @$ident"+s"_internal($argForInternal)\n"
        if(retTyvar){
          value += s"\t%ret.tyvar = load %tyvar* %ret.ptr\n"
          value += s"\t%ret.addr = extractvalue %tyvar %ret.tyvar, 0\n"
          value += s"\t%ret.type = extractvalue %tyvar %ret.tyvar, 1\n"
          value += s"\t%ret.mask = call %int @mask(%int %ret.addr, %int %ret.type)\n"
        }else{
          value += s"\t%ret.int = ptrtoint $retTypeId %ret.ptr to %int\n"
          value += s"\t%ret.mask = call %int @mask(%int %ret.int, %int $retTypeInt)\n"
        }
        value += s"\t;Switch stack, clear registers and flags\n"
        value += s"\tret %int %ret.mask\n\n"
      }else{
        value += s"\t%ret = call $retTypeId @$ident"+s"_internal($argForInternal)\n"
        value += s"\t;Switch stack, clear registers and flags\n"
        value += s"\tret %int %ret\n\n"
      }

      value += "\tError:\n" +
               "\t\tcall void @exit(i32 -1)\n" +
               "\t\tunreachable\n"

      value += s"}\n\n"

      val entrypoint =  s"define %int @$ident($argumentsAsInt){\n"+
                        s"\t;add entry point in spm\n"+
                        s"\t %ret = tail call %int @${ident}_stub($argumentsAsInt) ; Tail call\n"+
                        s"\t ret %int %ret\n" +
                        s"}\n\n"

      translation.entryPoints = translation.entryPoints.concat(entrypoint)
    }

    translation.values = translation.values.concat(value)

    return translation
  }

  def accessTyvars(arguments:List[Type], variables:List[Ident], trans:Translation, structure:Structure, prefix:String):String = {
    var access = ""
    var representatives = Map[Ident, String]();
    for((t,loc) <- arguments.zip(0 to arguments.length-1))
    {
      t match{
        case VarType(ident) => {
          val argName = if(prefix == "") variables(loc).value else "ML.typecheck"

          val tyvarName = s"%ML.typecheck.$prefix$loc"

          access += s"\t$tyvarName = bitcast i8* %$argName.tyvar.addr to %tyvar*\n"

          //Write the check!
          if(representatives.keySet.contains(ident)){
            val representative = representatives.get(ident).get
            access += s"\tcall i1 @tyvarcheck(%tyvar* $representative, %tyvar* $tyvarName)\n"
          }else{
            representatives = representatives.updated(ident, tyvarName)
          }
        }
        case StructType(_,0,_) => {} //reeds gechecked
        case s@StructType(ident,amount,types) => {
            val argName = variables(loc).value

            val structTypeString = s.qualifiedName(structure)
            val structTypeInt = trans.getOpaqueType(structTypeString)
            val implTypInt = trans.getImplTypeInt(structTypeInt)
            val implType = trans.getImplType(structTypeInt)//;.getVarTypeStr(trans);

            var packed = implType;
            var tyvars = types;
            var prefixrec = s"$prefix$loc";

            access += s"\t%ML.typecheck.$prefixrec.addr = add %int 0, %$argName.addr\n" //Bootstrapping

            def recFunc():Int = //Loopt door een effectieve structuur heen en matcht tyvars en implementaties.
            {
              packed match{
                case VarType(ident) => {
                  val tyvarName = s"%ML.typecheck.$prefixrec.tyvar"

                  access += s"\t$tyvarName = inttoptr %int %ML.typecheck.$prefixrec.addr to %tyvar*\n" // bitcast i8*, nu inttoptr

                  val assumedType = tyvars(ident.ord())

                  assumedType match{
                    case VarType(ident) => {
                      if(representatives.keySet.contains(ident)){//incorrect code
                      val representative = representatives.get(ident).get
                        access += s"\tcall i1 @tyvarcheck(%tyvar* $representative, %tyvar* $tyvarName)\n"
                      }else{
                        representatives = representatives.updated(ident, tyvarName)
                      }
                    }
                    case Integer => {
                      //check whether this is an Integer indeed!
                      access += s"\t$tyvarName.tyvar.val = load %tyvar* $tyvarName\n"
                      access += s"\t$tyvarName.tyvar.type = extractvalue %tyvar $tyvarName.tyvar.val, i32 1\n"
                      access += s"\t$tyvarName.tyvar.check = icmp eq %int ${Integer.intRepresentation}, $tyvarName.tyvar.type\n"
                      access += s"\tbr i1 $tyvarName.tyvar.check, label %Continue${prefixrec}int, label %Error \n"
                      access += "\n"
                      access += s"\t%Continue${prefixrec}int:\n"
                    }
                    case s2@StructType(ident2, amount2, types2) =>
                    {
                      //TODO! Nakijken of struct klopt -> Done
                      access += s"\t$tyvarName.tyvar.val = load %tyvar* $tyvarName\n"
                      access += s"\t$tyvarName.tyvar = extractvalue %tyvar $tyvarName.tyvar.val, i32 0\n"
                      access += s"\t$tyvarName.tyvar.type = extractvalue %tyvar $tyvarName.tyvar.val, i32 1\n"
                      access += s"\t$tyvarName.tyvar.check = icmp eq %int ${trans.getOpaqueType(s2.qualifiedName(structure))}, $tyvarName.tyvar.type\n"
                      access += s"\tbr i1 $tyvarName.tyvar.check, label %Continue${prefixrec}int, label %Error \n"
                      access += "\n"
                      access += s"\t%Continue${prefixrec}int:\n"



                      // Daarna recFunc() aanroepen met overschrijven van packed, prefixrec én van types
                      tyvars = types2;//adjust types
                      packed = trans.getImplType(trans.getOpaqueType(s2.qualifiedName(structure)))//New packed structure
                      prefixrec = s"${prefixrec}.${ident2.value}" // New prefix

                      access += s"\t%ML.typecheck.$prefixrec.addr = add %int 0, $tyvarName.tyvar.addr ;Bootstrapping \n" //TODO: REBOOTSTRAPPING -> Done
                      recFunc()
                    }
                    case _ => {throw new UnsupportedOperationException("Argument type not Integer/Struct/Var")}
                    //Als dit geen vartype of int is maar een structtype
                  }
                }
                //case StructType()
                case PairType(left, right) => {
                  access += s"\t%ML.typecheck.$prefixrec.pair = inttoptr %int %ML.typecheck.$prefixrec.addr to %pair*\n"
                  access += s"\t%ML.typecheck.$prefixrec.pair.val = load %pair* %ML.typecheck.$prefixrec.pair\n"
                  access += s"\t%ML.typecheck.$prefixrec.l = extractvalue %pair %ML.typecheck.$prefixrec.pair.val, 0\n" //tyvar*
                  access += s"\t%ML.typecheck.$prefixrec.r = extractvalue %pair %ML.typecheck.$prefixrec.pair.val, 0\n" //tyvar*
                  access += untyvarize(s"%ML.typecheck.$prefixrec.0", s"%ML.typecheck.$prefixrec.l", left, structure)
                  access += untyvarize(s"%ML.typecheck.$prefixrec.1", s"%ML.typecheck.$prefixrec.r", left, structure)

                  //access += s"\t%ML.typecheck.$prefixrec.0.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 0 ; tyvar**\n"
                  //access += s"\t%ML.typecheck.$prefixrec.1.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 1 ; tyvar**\n"

                  val prefixL = s"${prefixrec}.0"
                  val prefixR = s"${prefixrec}.1"


                  prefixrec = prefixL
                  packed = left
                  recFunc()

                  prefixrec = prefixR
                  packed = right
                  recFunc()
                }
                case ListType(inner) => { //TODO:Nakijken!
                    access += s"\t%ML.typecheck.$prefixrec.array = inttoptr %int %ML.typecheck.$prefixrec.addr to %array*\n"
                    access += s"\t%ML.typecheck.$prefixrec.array.val = load %array* %ML.typecheck.$prefixrec.array\n"
                    access += s"\t%ML.typecheck.$prefixrec.array.length = extractvalue %array %ML.typecheck.$prefixrec.array.val, 0\n"
                    access += s"\t%ML.typecheck.$prefixrec.check = icmp eq %int 0, %ML.typecheck.$prefixrec.array.length\n"
                    access += s"\tbr i1 %ML.typecheck.$prefixrec.check, label %Skip$prefixrec, label %Continue$prefixrec\n\n"

                    access += s"\tContinue$prefixrec:\n"
                    access += s"\t%ML.typecheck.$prefixrec.tyvar.ptr = getelementptr %array* %ML.typecheck.$prefixrec.array, i32 0, i32 1, %int 1;First Element: tyvar**\n"
                    access += s"\t%ML.typecheck.$prefixrec.tyvar = load %tyvar** %ML.typecheck.$prefixrec.tyvar.ptr ; tyvar*\n"
                    access += untyvarize(s"${prefixrec}.elem", s"%ML.typecheck.$prefixrec.tyvar", inner, structure) //TODO: Wat als hier nu een int in zit?

                    prefixrec = s"${prefixrec}.elem"
                    packed = inner
                    recFunc()

                    access += s"br label %Skip$prefixrec\n\n"
                    access += s"Skip$prefixrec:\n"
                }
                case _ => {throw new UnsupportedOperationException("Argument type not Integer/Struct/Var")} //throw new UnsupportedOperationException()
              }
              return 0
            }

            recFunc()
        }
        case _ => {} //Only Integers, because no pairs or arrays can be argument types! :) So no work needed!
      }
    }

    return s"$access\n"
  }

  def translateValue(trans:Translation, valdef:ValDefinition, structure:Structure, signature:Signature):Translation = {
    var translation = trans

    val valdefident = valdef.ident

    val declaration = signature.value.find{case ValDeclaration(`valdefident`,_)=>true; case _ => false}

    val ident = structure.ident.value + "." + valdef.ident.value;
    val returnInt = valdef.ascription == Integer;
    val typeId = valdef.ascription.getTypeString(structure)
    val typeInt = valdef.ascription match{
      case s@StructType(ident,_,_s) => trans.getOpaqueType(s.qualifiedName(structure))
      case Integer => Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("val expression with function type is not allowed.")}
      case _ => valdef.ascription.intRepresentation
    }

    //Internal function output
    var value = s"define private $typeId @$ident"+s"_internal(){\n"
    value += translateBody(trans, valdef, structure, signature, List(), typeId)
    value +="}\n\n"

    //External function output
    if(declaration.isDefined){
      value += s"define %int @${ident}_stub() noinline {\n"
      value += s"\t;Switch stack, move parameters\n"
      if(!returnInt){
        value += s"\t%ret.ptr = call $typeId @$ident"+s"_internal()\n"
        value += s"\t%ret.int = ptrtoint $typeId %ret.ptr to %int\n"
        value += s"\t%ret.mask = call %int @mask(%int %ret.int, %int $typeInt)\n"
        value += s"\t;Switch stack, clear registers and flags\n"
        value += s"\tret %int %ret.mask\n"
      }else{
        value += s"\t%ret = call $typeId @$ident"+s"_internal()\n"
        value += s"\t;Switch stack, clear registers and flags\n"
        value += s"\tret %int %ret\n"
      }
      value += s"}\n\n"

      val entrypoint = s"define %int @$ident(){\n"+
        s"\t;add entry point in spm\n"+
        s"\t %ret = tail call %int @${ident}_stub() ; Tail call\n"+
        s"\t ret %int %ret\n" +
        s"}\n\n"

      translation.entryPoints = translation.entryPoints.concat(entrypoint)
    }

    translation.values = translation.values.concat(value)

    return translation
  }

  def translateBody(trans:Translation, definition:Definition, structure:Structure, signature:Signature, argTypes:List[(Type, String)], retType:String):String = {
    var body = ""
    var types = Map[String, Type]()
    var lets = Map[Ident,String]()
    var temp = 0

    for(arg <- argTypes)
      arg match{
        case (argType, argName) => {

          val argTypeStr = argType.getTypeString(structure)

          if(argType == Integer){
            body += s"\t${argName}.val = add %int 0, ${argName} ; Integer reassign hack\n"
          }else{
            body += s"\t${argName}.val = load ${argTypeStr} ${argName}\n"
          }

          types = types + (s"${argName}" -> argType)
        }
      }


    val exp = definition match{
      case FunDefinition(_,args,_,expression) => expression
      case ValDefinition(_,_,expression) => expression
      case _ => {throw new UnsupportedOperationException()}
    }

    def localize(origin: String, typ:Type):String = typ match{
      case StructType(id,_,_) => {
        if(id.value.contains(".")){
          s"%$id*"
        }else{
          s"%${origin}.${id.value}*"
        }
      }
      case Integer => Integer.toString()
      case x@_ => s"${x.toString()}*"
    }

    def translateExp(exp:Expr):String = {
      exp match{
        case ConstExpr(value) => {
          body += s"\t%t$temp = add %int 0, ${value} ; Hack to give ints a name \n "

          types = types + (s"%t$temp" -> Integer)
          temp = temp + 1
          s"%t${temp-1}"
        }
        case CallExpr(name, args) => {
         val funcName = name.internalize(structure) //Find the function name. Either it is local, or it already is a qualified name
         val origin = funcName.split("""\.""")(0) //Get the qualification part from the func name. This is the defining structure for any type in

         val funcType = trans.getFuncType(structure, name); //Find the function type associated with the function. Pass the current structure, because the function name might not be a qualified name.

         val argIds = args.map(argExp => translateExp(argExp)) //Translate the expressions and give back the bindings, example: //List("%t1", "%t2")
         val argData = funcType.left.zip(argIds) // List[(Expected Type, LLVM Identifier pre conversion)]
         var argCallData = List[(String, String)]() //List[(Expected Type in String, LLVM Identifier post conversion)]

         //Possibly, these values are (according to LLVM) not yet of the right type. In that case, we need to cast them.
         for((expectedType, identifier) <- argData){
           if(types.get(identifier).get != expectedType){
             if(expectedType == Integer){ // If the expected type is an integer, the current type MUST be of type {%int}, so extract the int.
              body += "\n"
              body += s"\t%t${temp}.${identifier.substring(1)}.val = load ${types.get(identifier).get.getTypeString(structure)} ${identifier} \n"
              body += s"\t%t${temp}.${identifier.substring(1)} = extractvalue %${types.get(identifier).get.qualifiedName(structure)} %t${temp}.${identifier.substring(1)}.val, 0 \n"
             }else if(expectedType.isInstanceOf[VarType]){ // If the expected type is a type variable, tyvarize it. We already are sure the current type associated with the identifier isn't a typevar.
               body += "\n" + tyvarize(s"%t${temp}.${identifier.substring(1)}", identifier, types.get(identifier).get, structure) + "\n"
             }else{ //bitcast.
               body += "\n" + s"\t%t${temp}.${identifier.substring(1)} = bitcast ${types.get(identifier).get.getTypeString(structure)} ${identifier} to ${localize(origin,expectedType)}\n"+ "\n"
             }
             argCallData = argCallData :+ (localize(origin,expectedType), s"%t${temp}.${identifier.substring(1)}")// build call data with adjusted identifier //De funcType annotatie komt van structure met naam origin, niet perse huidige structure
           }else{
             argCallData = argCallData :+ (localize(origin,expectedType), identifier)// build call data with unchanged identifier //De funcType annotatie komt van structure met naam origin, niet perse huidige structure
           }
         }

         val argCallString = argCallData.map{case (a,b) => s"$a $b"}.mkString(", ") //build the argument string

         body += s"\t%t$temp = call ${localize(origin, funcType.right)} @${funcName}_internal(${argCallString})\n" //perform the call

         types = types + (s"%t$temp" -> funcType.right) //TODO: Might need to change ident to the qualified version?
         temp = temp+1
         return s"%t${temp-1}"
        }
        case BinOpExpr(op, left, right) => {
          var leftbind = translateExp(left)
          var rightbind = translateExp(right)

          if(types.get(leftbind).get != Integer){
            body += s"\t%t${temp}.left.int = extractvalue %${types.get(leftbind).get.qualifiedName(structure)} $leftbind.val, 0 ; Access \n"
            leftbind = s"%t${temp}.left.int"
          }

          if(types.get(rightbind).get != Integer){
            body += s"\t%t${temp}.right.int = extractvalue %${types.get(rightbind).get.qualifiedName(structure)} $rightbind.val, 0 ; Access\n"
            rightbind = s"%t${temp}.right.int"
          }

          op match{
            case Add => body += s"\t%t$temp = add %int $leftbind, $rightbind\n"
            case Sub => body += s"\t%t$temp = sub %int $leftbind, $rightbind\n"
            case Mul => body += s"\t%t$temp = mul %int $leftbind, $rightbind\n"
            case Rem => body += s"\t%t$temp = srem %int $leftbind, $rightbind\n"
          }

          types = types + (s"%t$temp" -> Integer)
          temp = temp+1
          return s"%t${temp-1}"
        }
        case LetExpr(ident, bind, in) => {
          val bnd = translateExp(bind);
          lets = lets + (ident->bnd)
          return translateExp(in)
        }
        case ValExpr(ident) =>{

          if(lets.contains(ident)){ //Bound by a local let expression
            return lets.get(ident).get
          }else if(argTypes.map{case (x,y)=>y; case _ => {}}.contains(s"%${ident.value}")){ //One of the arguments
            //val name = s"%${ident.value}"
            return s"%${ident.value}"//return argTypes.find{case (_,`name`) => true; case _ => false}.get._2
          }else{ //Bound by this structure or another structure
            val valName = ident.internalize(structure)
            val origin = valName.split("""\.""")(0)

            val valType = trans.getValType(structure, ident)
            body += s"\t%t$temp = call ${localize(origin, valType)} @${valName}_internal()\n"

            //load value
            if(valType == Integer){
              body +=s"\t%t$temp.val = add %int 0, %t$temp\n"
            }else{
              body +=s"\t%t$temp.val = load ${localize(origin, valType)} %t$temp\n"
            }

            types = types + (s"%t$temp" -> valType)
            temp = temp + 1 //hoog temp op

            return s"%t${temp-1}"
          }
        }
        case PairExpr(left,right) => {
          val leftbind = translateExp(left)
          val rightbind = translateExp(right)

          body += s"\t%t${temp}.addr = call i8* @malloc(%int 16)\n"
          body += s"\t%t${temp} = bitcast i8* %t${temp}.addr to %pair*\n"

          if(types.get(leftbind).get.isInstanceOf[VarType]){
            body += s"\t%t${temp}.0 = insertvalue %pair undef, %tyvar* ${leftbind},0  ; insert left tyvar in pair\n"
          }else{
            body += tyvarize(s"%t${temp}.l", leftbind, types.get(leftbind).get, structure)
            body += s"\t%t${temp}.0 = insertvalue %pair undef, %tyvar* %t${temp}.l, 0 ; insert left tyvar in pair\n" //TODO
          }

          if(types.get(rightbind).get.isInstanceOf[VarType]){
            body += s"\t%t${temp}.1 = insertvalue %pair %t${temp}.0, %tyvar* ${rightbind},1 ; insert right tyvar in pair\n"
          }else{
            body += tyvarize(s"%t${temp}.r", rightbind, types.get(rightbind).get, structure)
            body += s"\t%t${temp}.0 = insertvalue %pair %t${temp}.0, %tyvar* %t${temp}.r, 1 ; insert right tyvar in pair\n" //TODO
          }

          body += s"\tstore %pair %t${temp}.1, %pair* %t${temp} ; Store pair to pointer\n"
          body += s"\t%t${temp}.val = load %pair* %t${temp}  ; load pair from pointer for later use\n"

          types = types + (s"%t$temp" -> PairType(types.get(leftbind).get,types.get(rightbind).get))
          temp = temp+1
          return s"%t${temp-1}"
        }
        case PairAccess(pair, annot, side) => {
          val pairbnd = translateExp(pair)

          //bitcast to pair* if type not pair* //TODO: extractable
          if(! types.get(pairbnd).get.isInstanceOf[PairType]){ //If the bound var is of a different type than the pair type -> LLVM code to convert
            if(types.get(pairbnd).get.isInstanceOf[TypeVar]){ // If it is a tyvar, load the tyvar, get the address as an int, and cast it to a pair.
              body +=s"\t%t${pairbnd}.${temp}.val = load %tyvar* $pairbnd\n"
              body +=s"\t%t${temp}.pair.addr = extractvalue %tyvar $pairbnd.${temp}.val, 0 ; extract address from tyvar\n"
              body +=s"\t%t${temp}.pair = inttoptr %int %t${temp}.pair.addr to %pair* ; cast address to pair*\n" // bitcast -> inttoptr
            }else{ //If it is not a tyvar, it must be a pointer type, which is bitcastable to a pair* pointer.
              body +=s"\t%t${temp}.pair = bitcast ${types.get(pairbnd).get.getTypeString(structure)} $pairbnd to %pair*\n" //Changed + *, the original type can never be a non-pointer type
            }
            body += s"\t%t${temp}.pair.val = load %pair* %t${temp}.pair\n"
          }else{
            body += s"\t%t${temp}.pair.val = load %pair* %t${pairbnd}\n"
          }

          body += s"\t%t${temp}.tyvar.ptr = extractvalue %pair %t${temp}.pair.val, ${side.loc}\n" // This loads the %tyvar pointer value inside the pair
          //body += s"\t%t${temp}.tyvar.ptr = inttoptr %int %t${temp}.tyvar.addr to %tyvar*\n" //Changed bitcast -> inttoptr

          body += untyvarize(s"%t${temp}", s"%t${temp}.tyvar.ptr", annot, structure)

          types = types + (s"%t$temp" -> annot)
          temp = temp+1;
          return s"%t${temp-1}"
        }
        case Empty(annot) => {
          body +=s"\t%t${temp}.addr = call i8* @malloc(%int 1600)\n"
          body +=s"\t%t${temp} = bitcast i8* %t${temp}.addr to %array*\n"
          body +=s"\t%t${temp}.val.0 = insertvalue %array undef, %int 0, 0\n"
          body +=s"\t%t${temp}.val.1 = insertvalue %array %t${temp}.val.0, %tyvar* null,1\n"
          body +=s"\tstore %array %t${temp}.val.1, %array* %t${temp}\n"

          types = types + (s"%t${temp}" -> ListType(annot))
          temp = temp+1
          return s"%t${temp-1}"
        }
        case ConsExpr(elem:Expr, tail:Expr) => {

          var elembnd = translateExp(elem)
          val tailbnd = translateExp(tail)
          val arraybnd = s"%t${temp}"


          body += getAsArray(s"%t${temp}", tailbnd, types.get(tailbnd).get, structure)
          temp = temp +1

          body += s"\t%t${temp}.array.length = extractvalue %array ${arraybnd}.val, 0\n"
          body += s"\t%t${temp}.array.elem.ptr = getelementptr %array* ${arraybnd}, i32 0, i32 1, %int %t${temp}.array.length\n"

          if(types.get(elembnd).get.isInstanceOf[VarType]){
            body += s"store %tyvar* ${elembnd}, %tyvar** %t${temp}.array.elem.ptr\n"
          }else{
            body += tyvarize(s"%t${temp}.elem", elembnd, types.get(elembnd).get, structure)
            body += s"\tstore %tyvar* %t${temp}.elem, %tyvar** %t${temp}.array.elem.ptr\n"
          }

          body += s"\t%t${temp}.array.length.ptr = getelementptr %array* ${arraybnd}, i32 0, i32 0\n"
          body += s"\t%t${temp}.array.length.new = add %int 1, %t${temp}.array.length\n"
          body += s"\tstore %int %t${temp}.array.length.new, %int* %t${temp}.array.length.ptr\n"

          temp = temp+1

          return tailbnd
        }
        case ListIndex(list:Expr, index:Int, annot:Type) => {
          var listbnd = translateExp(list)

          body += getAsArray(s"%t${temp}", listbnd, types.get(listbnd).get, structure)
          listbnd = s"%t${temp}"

          temp = temp+1

          body += s"\t%t${temp}.array.length = extractvalue %array %t${listbnd}.val, 0\n"
          body += s"\t%t${temp}.check = icmp slt %int $index, %t${temp}.array.length\n"
          body += s"\tbr i1 %t${temp}.check, label %Continue$temp, label %Exit$temp \n\n"

          body += s"\tError$temp:\n" +
                  "\t\tcall void @exit(i32 -1)\n" +
                  "\t\tunreachable\n\n"

          body += s"Continue${temp}:\n"
          body += s"\t%t${temp}.tyvar.ptr.ptr = getelementptr %array* $listbnd, i32 0, i32 1, %int $index\n"
          body += s"\t%t${temp}.tyvar.ptr = load %tyvar** %t${temp}.tyvar.ptr.ptr\n"

          body += untyvarize(s"%t${temp}", s"%t${temp}.tyvar.ptr", annot, structure)

          types = types + (s"%t${temp}" -> annot)
          temp = temp+1
          return s"%t${temp-1}"

        }
        case expr@_ => {throw new UnsupportedOperationException(s"Expression $expr could not be handled")}
      }
    }


    var retval = translateExp(exp)

    if(types.get(retval).get.getTypeString(structure) != retType){
      if(types.get(retval).get == Integer){
        body += s"\t%t$temp.addr = call i8* @malloc(%int 8)\n"
        body += s"\t%t$temp = bitcast i8* %t$temp.addr to ${retType}\n"
        body += s"\t%t$temp.val = insertvalue ${retType.replace("*","")} undef, %int $retval, 0\n"
        body += s"\tstore ${retType.replace("*","")} %t$temp.val, ${retType} %t$temp\n"
        retval = s"%t$temp"
      }else{
        if(retType == Integer.toString()){
          body += s"\t%t$temp.val = load ${types.get(retval).get.getTypeString(structure)} $retval\n"
          body += s"\t%t$temp = extractvalue %${types.get(retval).get.qualifiedName(structure)} %t$temp.val, 0\n"
          retval = s"%t$temp"
          //body += s"\t%t$temp = bitcast ${types.get(retval).get.getTypeString(structure)} $retval.val to $retType\n"

        } else{
          body += s"\t%t$temp = bitcast ${types.get(retval).get.getTypeString(structure)} $retval to $retType\n"
          retval = s"%t$temp"
        }
      }
    }
    body += s"\tret $retType $retval\n"

    return body
  }

  def getAsArray(desiredName:String, currentName:String, currentType:Type, structure:Structure):String = {
    var body = ""
    if(!currentType.isInstanceOf[ListType]){ //If the tail is of a different type than List type -> LLVM code to convert
      if(currentType.isInstanceOf[TypeVar]){  //If it is a tyvar, get the
        body +=s"\t${desiredName}.tyvar.val = load %tyvar* $currentName\n"
        body +=s"\t${desiredName}.addr = extractvalue %tyvar ${desiredName}.tyvar.val, 0 ; extract address from tyvar\n"
        body +=s"\t${desiredName} = inttoptr %int ${desiredName}.array.addr to %array* ; cast address to array*\n" // bitcast -> inttoptr
      }else{
        body +=s"\t${desiredName} = bitcast ${currentType.getTypeString(structure)} $currentName to %array*\n" //Added *, the original type can never be a non-pointer type
      }
      body += s"\t${desiredName}.val = load %array* %t${desiredName}\n"
    }else{
      body += s"\t${desiredName}.addr = ptrtoint %array* $currentName to %int\n"
      body += s"\t${desiredName} = inttoptr %int ${desiredName}.addr to %array*\n"
      body += s"\t${desiredName}.val = load %array* ${currentName}\n"
    }
    return body
  }

  // @param desiredName     String containing the identifier that the tyvar* pointer should be bound to eventually.
  // @param valueToTyvarize String containing the identifier that the value to tyvarize is currently bound to
  // @param currentType     The current type of the value.
  // @param structure       The 'local environment' structure.
  def tyvarize(desiredName:String, valueToTyvarize:String, currentType:Type, structure:Structure):String = {
    var body = ""
    body += s"\t${desiredName}.addr = call i8* @malloc(%int 16)\n"
    body += s"\t${desiredName} = bitcast i8* ${desiredName}.addr to %tyvar*\n"
    if(currentType != Integer){
      body += s"\t${desiredName}.int = ptrtoint ${currentType.getTypeString(structure)} ${valueToTyvarize} to %int; cast ptr to int\n" //added *
      body += s"\t${desiredName}.0 = insertvalue %tyvar undef, %int ${desiredName}.int, 0 ; create tyvar step 1\n "
    }else{
      body += s"\t${desiredName}.0 = insertvalue %tyvar undef, %int ${valueToTyvarize}, 0 ; create tyvar step 1\n "
    }
    body += s"\t${desiredName}.1 = insertvalue %tyvar ${desiredName}.0, %int ${currentType.intRepresentation}, 1 ; create tyvar step 2\n "
    body += s"\tstore %tyvar ${desiredName}.1, %tyvar* ${desiredName} ; Store the tyvar \n" //save

    return body;
  }

  def untyvarize(desiredName:String, tyvarPtr:String, annot:Type, structure:Structure):String = {
    var body = ""

    if(annot.isInstanceOf[VarType]){
      body += s"\t${desiredName}.addr = ptrtoint %tyvar* ${tyvarPtr} to %int\n"
      body += s"\t${desiredName} = inttoptr %int ${desiredName}.addr to %tyvar*\n"
      body += s"\t${desiredName}.val = load %tyvar* ${desiredName} \n"
    } else {
      body += s"\t${desiredName}.tyvar = load %tyvar* ${tyvarPtr}\n"

      if (annot == Integer) {
        body += s"\t${desiredName} = extractvalue %tyvar ${desiredName}.tyvar, 0\n"
        body += s"\t${desiredName}.val = extractvalue %tyvar ${desiredName}.tyvar, 0\n"
      } else {
        body += s"\t${desiredName}.addr = extractvalue %tyvar ${desiredName}.tyvar, 0\n ; int"
        body += s"\t${desiredName} = inttoptr %int ${desiredName}.addr to ${annot.getTypeString(structure)}\n"
        body += s"\t${desiredName}.val = load ${annot.getTypeString(structure)} ${desiredName}\n" //Load the value.
      }
    }

    return body
  }
}
//Zou ook rekening kunnen houden bij call om dan te checken of het unexpected type miss een tyvar is, en dan geen annotaties nodig bij het uitpakken van een pair/array, omdat je tyvar kan gebruiken. Then again, wat met tyvars van een verschillend type? Welke 'ident'?
//Implementation type must be Liskov substitution principle subtype of declaration type.
// For functions:
//    1. implementation has tyvar argument while declaration specifies specific type. (Contravariance)
//    2. implementation has specific returntype while declaration is more general. (Covariance)
// This has already mostly been implemented because we tyvarize an argument when it isn't a tyvar when it's supposed to. (Contravariance)
// TODO: The second doesn't really happen in normal situations? (i.e. you never have return type typevar a, unless a was featured in the type earlier. But then it's an argument, and this can't be replaced by the more specific type, as it breaks contravariance)
