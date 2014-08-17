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
  override def toString() = types + "\n" + values;

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

      val defStruct = program.structures.find{case Structure(`defStructIdent`,_,_) => true ; case _ => false}.get
      val sigId = defStruct.signature
      val defSignature = program.signatures.find{case Signature(`sigId`,_) => true; case _ => false}.get
      return defSignature.value.find{case ValDeclaration(`valueIdent`,_) => true; case _ => false}.get.asInstanceOf[ValDeclaration].ascription.asInstanceOf[FuncType]
    }else{
      return structure.value.find{case FunDefinition(`search`,_,_,_) => true ;case _ => false}.get.asInstanceOf[FunDefinition].ascription
    }
  }
}

sealed trait Type{
  def intRepresentation:Int
  def getVarTypes(trans:Translation):List[Int]
}

case object Integer extends Type {
  override def toString() = "%int"
  def intRepresentation = 1;
  override def getVarTypes(trans:Translation):List[Int] = {
    return List()
  }
}

case class FuncType(left:List[Type], right:Type) extends Type{
  def intRepresentation = 5;
  override def getVarTypes(trans:Translation):List[Int] = {
    throw new UnsupportedOperationException();
  }
}

case class VarType(ident:Ident) extends Type {
  override def toString() = "%tyvar"
  def intRepresentation = 2;

  override def getVarTypes(trans:Translation):List[Int] = {
    return List(ident.value.charAt(0).toInt-97)
  }
}

case class StructType(typeId:Ident, nrTyvars:Int, tyvars:List[Type]) extends Type {
  def intRepresentation = -1; //Hmm

  def internalize(structure:Structure):String = {
    if(typeId.value.contains("."))
      return typeId.value;
    else
      return s"${structure.ident.value}.${typeId.value}"
  }

  def getPtrStr(structure:Structure):String = {
    return s"%${internalize(structure)}*"
  }

  override def getVarTypes(trans:Translation):List[Int] = {
    return trans.getImplType(trans.getOpaqueType(typeId.value)).getVarTypes(trans)
  }

  //override def toString() = typeId.value

}

case class PairType(left:Type, right:Type) extends Type {
  override def toString() = "%pair"
  def intRepresentation = 3;
  override def getVarTypes(trans:Translation):List[Int] = {
    return left.getVarTypes(trans)++right.getVarTypes(trans)
  }
}

case class ListType(inner:Type) extends Type {
  override def toString() = "%array"
  def intRepresentation = 4;

  override def getVarTypes(trans:Translation):List[Int] = {
    return inner.getVarTypes(trans)
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
case class LeftExpr(pair:Expr) extends Expr
case class RightExpr(pair:Expr) extends Expr
sealed trait ListExpr extends Expr
case object Empty extends Expr
case class consExpr(tail:Expr) extends Expr

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
                                      new OpaqueTypeDeclaration(new Ident("pair"), 1),
                                      new ValDeclaration(new Ident("newcredentials"), new StructType(new Ident("cred"),0, List())),
                                      new ValDeclaration(new Ident("encrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"),0, List())), Integer)),
                                      new ValDeclaration(new Ident("decrypt"), new FuncType(List(Integer, new StructType(new Ident("cred"),0, List())), Integer)),
                                      new ValDeclaration(new Ident("createPair"), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"),1, List(new VarType(new Ident("a")))))),
                                      new ValDeclaration(new Ident("getLeft"), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a")))),
                                      new ValDeclaration(new Ident("merge"), new FuncType(List(paira, paira), paira))
                                 )
                                 )

  val paira = new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))
  val caesar = new  Structure(new Ident("Caesar"), new Ident("SYMMETRICCIPHER"),
                      List(
                        new TypeDefinition(new Ident("cred"), 0, Integer),
                        new TypeDefinition(new Ident("pair"), 1, PairType(new VarType(new Ident("a")), new VarType(new Ident("a")))),
                        new ValDefinition(new Ident("newcredentials"), new StructType(new Ident("cred"), 0, List()), ConstExpr(3)),
                        new FunDefinition(new Ident("encrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"), 0, List())), Integer), ConstExpr(3)),
                        new FunDefinition(new Ident("decrypt"), List(new Ident("a"), new Ident("cred")), new FuncType(List(Integer, new StructType(new Ident("cred"), 0, List())), Integer), ConstExpr(3)),
                        new FunDefinition(new Ident("createPair"), List(new Ident("left"), new Ident("right")), new FuncType(List(new VarType(new Ident("a")),new VarType(new Ident("a"))), new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), ConstExpr(3)),
                        new FunDefinition(new Ident("getLeft"), List(new Ident("pair")), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))), CallExpr(new Ident("createPair"),List(ConstExpr(3)))), // ConstExpr(3)),
                        new FunDefinition(new Ident("merge"), List(new Ident("left"), new Ident("right")), new FuncType(List(paira, paira), paira), ConstExpr(3)),
                        new ValDefinition(new Ident("seed"), Integer, ConstExpr(3)),
                        new ValDefinition(new Ident("rand"), Integer, ConstExpr(3))
                      )
                    )

  val caesar2 = new Structure(new Ident("ACaesar"), new Ident("SYMMETRICCIPHER"), List())

  val program = new Program(List(symmetriccipher), List(caesar, caesar2))

  println(translate(program))

  var entrypoints:String = ""

  def translate(program:Program):Translation={
    var translation = new Translation(program,"", "", "",true, 5, Map(), List(), Map());

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

    val declaration = signature.value.find{case OpaqueTypeDeclaration(typedef.ident,_)=>true; case _ => false}

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

    val retTypeId = fundef.ascription.right match{
      case s@StructType(ident,_,_) => s.getPtrStr(structure) // + (if(ident.value.contains(".")) ident.value else structure.ident.value + "." + ident.value + "*")
      case Integer => Integer.toString()
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => fundef.ascription.right.toString() +"*"
    }
    val retTypeInt = fundef.ascription.right match{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.internalize(structure))
      case Integer => Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => fundef.ascription.right.intRepresentation
    }

    val argInt = fundef.ascription.left.map(x => x == Integer)
    val argTyvar = fundef.ascription.left.map(x => x.isInstanceOf[VarType])
    val argTypeId = fundef.ascription.left.map{
      case s@StructType(ident,_,_) => s.getPtrStr(structure)
      case Integer => Integer.toString()
      case FuncType(_,_) => {throw new Error("Functions not allowed as argument type.")}
      case x@_ => x.toString()+"*"
    }
    val argTypeInt = fundef.ascription.left.map{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.internalize(structure))
      case Integer => Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => fundef.ascription.right.intRepresentation
    }

    val argNames = fundef.variables.map{x=>"%"+x.value}
    val argForInternal = argTypeId.zip(argNames).map{x => x._1 + " "+x._2}.mkString(", ")

    //Internal function output
    var value = s"define private $retTypeId @$ident"+s"_internal($argForInternal){\n"
    value += translateBody(trans, fundef, structure, signature, argTypeId.zip(argNames), retTypeId)
    value +="}\n\n"

    //External function output
    if(declaration){
      val argumentsAsInt = argTyvar.zip(argNames).map{x => "%int "+x._2+".in" + (if(x._1){", i2 " + x._2 + ".isMask"}else{""})}.mkString(", ")

      value += s"define %int @$ident($argumentsAsInt){\n"
      value += s"\t;Switch stack, move parameters, add entry point in spm\n"

      val argInformation = (fundef.variables, 1 to fundef.variables.length,(argTyvar,argTypeId,argTypeInt).zipped.toList).zipped.toList
      val argProcessingCode = argInformation.map{
        case (name,nr,(false,typeId,1)) => s"\tProcess$nr:\n" +
                                           s"\t\t%$name = add %int %$name.in, 0 ; Renaming trick \n" +
                                           s"\t\tbr label Process${nr+1}\n\n"
        case (name,nr,(false,typeId,typeInt)) => s"\tProcess$nr:\n" +
                                                 s"\t\t%$name.addr = call %int @unmask(%int $name.in)\n" +
                                                 s"\t\t%$name.type = call %int @unmasktype(%int $name.mask)\n" +
                                                 s"\t\t%$name = inttoptr %int %$name.addr to $typeId\n" +
                                                 s"\t\t%$name.check = icmp ne %int %$name.type $typeInt\n" +
                                                 s"\t\tbr i1 %$name.check, label %Error, label Process${nr+1}\n\n"
        case (name,nr,(true,typeId, typeInt)) => s"\tProcess$nr:\n" +
                                                 s"\t\t%$name.tyvar.addr = call i8* @malloc(%int 16)\n" +
                                                 s"\t\t%$name = bitcast i8* %$name.tyvar.addr to %tyvar*\n" +
                                                 s"\t\tswitch i2 %$name.isMask, label %Unmask$nr [i2 0, label %External$nr\n" +
                                                 s"\t\t                                           i2 1, label %Int$nr]\n\n" +
                                                 s"\tUnmask$nr:\n" +
                                                 s"\t\t%$name.addr = call %int @unmask(%int %$name.in)\n" +
                                                 s"\t\t%$name.type = call %int @unmasktype(%int %$name.mask)\n" +
                                                 s"\t\t%$name.unmask.1 = insertvalue %tyvar undef, %int %$name.addr, 0\n" +
                                                 s"\t\t%$name.unmask.2 = insertvalue %tyvar %$name.unmask.1, %int %$name.type, 1\n" +
                                                 s"\t\tbr label %Create$nr\n\n" +
                                                 s"\tExternal$nr:\n" +
                                                 s"\t\t%$name.ext.1 = insertvalue %tyvar undef, %int %$name.in, 0\n" +
                                                 s"\t\t%$name.ext.2 = insertvalue %tyvar undef, %int 0, 1\n" +
                                                 s"\t\tbr label %Create$nr\n\n" +
                                                 s"\t\tInt$nr:\n" +
                                                 s"\t\t%$name.int.1 = insertvalue %tyvar undef, %int %$name.in, 0\n" +
                                                 s"\t\t%$name.int.2 = insertvalue %tyvar undef, %int 1, 1\n" +
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

            val structTypeString = s.internalize(structure)
            val structTypeInt = trans.getOpaqueType(structTypeString)
            val implTypInt = trans.getImplTypeInt(structTypeInt)
            val implType = trans.getImplType(structTypeInt)//;.getVarTypeStr(trans);

            var packed = implType;
            var tyvars = types;
            var prefixrec = s"$prefix$loc";

            access += s"\t%ML.typecheck.$prefixrec.addr = add %int 0, $argName.addr\n" //Bootstrapping

            def recFunc():Int = //Loopt door een effectieve structuur heen en matcht tyvars en implementaties.
            {
              packed match{
                case VarType(ident) => {
                  val tyvarName = s"%ML.typecheck.$prefixrec"

                  access += s"\t$tyvarName = bitcast i8* %ML.typecheck.$prefixrec.addr to %tyvar*\n"

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
                      access += s"\t$tyvarName.tyvar = load %tyvar* $tyvarName\n"
                      access += s"\t$tyvarName.tyvar.type = extractvalue %tyvar $tyvarName.tyvar, i32 1\n"
                      access += s"\t$tyvarName.tyvar.check = icmp eq %int ${Integer.intRepresentation}, $tyvarName.tyvar.type\n"
                      access += s"\tbr i1 $tyvarName.tyvar.check, label %Continue${prefixrec}int, label %Error \n"
                      access += "\n"
                      access += s"\t%Continue${prefixrec}int:\n"
                    }
                    case s2@StructType(ident2, amount2, types2) =>
                    {
                      //TODO! Nakijken of struct klopt -> Done
                      access += s"\t$tyvarName.tyvar = load %tyvar* $tyvarName\n"
                      access += s"\t$tyvarName.tyvar.addr = extractvalue %tyvar $tyvarName.tyvar, i32 0\n"
                      access += s"\t$tyvarName.tyvar.type = extractvalue %tyvar $tyvarName.tyvar, i32 1\n"
                      access += s"\t$tyvarName.tyvar.check = icmp eq %int ${trans.getOpaqueType(s2.internalize(structure))}, $tyvarName.tyvar.type\n"
                      access += s"\tbr i1 $tyvarName.tyvar.check, label %Continue${prefixrec}int, label %Error \n"
                      access += "\n"
                      access += s"\t%Continue${prefixrec}int:\n"



                      // Daarna recFunc() aanroepen met overschrijven van packed, prefixrec én van types
                      tyvars = types2;//adjust types
                      packed = trans.getImplType(trans.getOpaqueType(s2.internalize(structure)))//New packed structure
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
                  access += s"\t%ML.typecheck.$prefixrec.0.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 0 ; tyvar*\n"
                  access += s"\t%ML.typecheck.$prefixrec.1.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 1 ; tyvar*\n"

                  val prefixL = s"${prefixrec}.0"
                  val prefixR = s"${prefixrec}.1"


                  prefixrec = prefixL
                  packed = left
                  recFunc()

                  prefixrec = prefixR
                  packed = right
                  recFunc()
                }
                case ListType(inner) => {
                    access += s"\t%ML.typecheck.$prefixrec.array.ptr = inttoptr %int %ML.typecheck.$prefixrec.addr to %array*\n"
                    access += s"\t%ML.typecheck.$prefixrec.array = load %array* %ML.typecheck.$prefixrec.array.ptr\n"
                    access += s"\t%ML.typecheck.$prefixrec.array.length = extractvalue %array %ML.typecheck.$prefixrec.array, 0\n"
                    access += s"\t%ML.typecheck.$prefixrec.check = icmp eq %int 0, %ML.typecheck.$prefixrec.array.length\n"
                    access += s"\tbr i1 %ML.typecheck.$prefixrec.check, label %Skip$prefixrec, label %Continue$prefixrec\n\n"

                    access += s"\tContinue$prefixrec:\n"
                    access += s"\t%ML.typecheck.$prefixrec.0.addr = getelementptr %array %ML.typecheck.$prefixrec.array.ptr, i32 0, %int 1;First Element\n"

                    prefixrec = s"${prefixrec}.0"
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

    val declaration = signature.value.find{case ValDeclaration(valdef.ident,_)=>true; case _ => false}

    val ident = structure.ident.value + "." + valdef.ident.value;
    val returnInt = valdef.ascription == Integer;
    val typeId = valdef.ascription match{
      case s@StructType(ident,_,_) => s.getPtrStr(structure)
      case Integer => Integer.toString()
      case FuncType(_,_) => {throw new Error("val expression with function type is not allowed.")}
      case _ => valdef.ascription.toString() +"*"
    }
    val typeInt = valdef.ascription match{
      case s@StructType(ident,_,_s) => trans.getOpaqueType(s.internalize(structure))
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
      value += s"define %int @$ident(){\n"
      value += s"\t;Switch stack, move parameters, add entry point in spm\n"
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
    }

    translation.values = translation.values.concat(value)

    return translation
  }

//  def getVarTypeInformation(prefix:List[Int], argType:StructType, idents:Map[Ident,List[List[Int]]]):(Map[Ident,List[List[Int]]],List[(List[Int], Type)]) = {
//    var identMap = idents;
//    var instantiationsMap = List[(List[Int], Type)]() // This map contains instantiated tyvars of the parametric type.
//
//    for((argType, loc)<-argType.tyvars.zip(0 to argType.nrTyvars-1)){
//      argType match{
//        case VarType(ident) => {identMap = identMap.updated(ident, (prefix.:+(loc)) :: identMap.getOrElse(ident, List()))}
//        case x:StructType => { val pair = getVarTypeInformation((prefix.:+(loc)),x,identMap)
//          identMap = pair._1;
//          instantiationsMap.+:((prefix.:+(loc), x)).++(pair._2);
//        }
//        case _ => {}
//      }
//    }
//
//    return (identMap, instantiationsMap)
//  }

  def translateBody(trans:Translation, definition:Definition, structure:Structure, signature:Signature, argTypes:List[(String, String)], retType:String):String = {
    var body = ""
    for(arg <- argTypes)
      if(arg._1 == "%int")
        body += s"\t${arg._2}.val = add %int 0, ${arg._2} ; Integer reassign hack\n"
      else
        body += s"\t${arg._2}.val = load ${arg._1} ${arg._2}\n"

    val exp = definition match{
      case FunDefinition(_,args,_,expression) => expression
      case ValDefinition(_,_,expression) => expression
      case _ => {throw new UnsupportedOperationException()}
    }

    var temp = 0

    def translateExp(exp:Expr):String = {
      exp match{
        case ConstExpr(value) => { return "" }
        case CallExpr(name, args) => {
         val funcName = name.internalize(structure);
         val local = funcName.split("""\.""")(0)

         val funcType = trans.getFuncType(structure, name);

         def localize(typ:Type):String = typ match{
           case StructType(id,_,_) => {
             if(id.value.contains(".")){
               s"%$id*"
             }else{
               s"%${local}.${id.value}*"
             }
           }
           case Integer => Integer.toString()
           case x@_ => s"${x.toString()}*"
         }

         val argTypesCall = funcType.left.map(x => localize(x))
         val argIds = args.map(argExp => translateExp(argExp)) //Geeft hun bindings terug, bijv: //List("%t1", "%t2")
         val argCallString = argTypesCall.zip(argIds).map{case (a,b) => s"$a $b"}.mkString(", ")

         body += s"\t%t$temp = call ${localize(funcType.right)} @$funcName(${argCallString})\n"
         return s"%t$temp"
        }
        case BinOpExpr(op, left, right) => {
          
        }
        case _ => {return ""}
      }
    }

    exp match {
      case ConstExpr(value) => body += s"\treturn %int $value\n"
      case call @ CallExpr(name, args) =>{
        val retval = translateExp(call);
        body += s"\t return $retType $retval\n"
      }
      case _ => {}
    }

    return body
  }

//  sealed trait Expr
//  case class ValExpr(ident:Ident) extends Expr
//  case class ConstExpr(value: Int) extends Expr
//  case class CallExpr(name:Ident, args: List[Expr]) extends Expr
//  case class BinOpExpr(op:BinOp, left:Expr, right:Expr) extends Expr
//  case class LetExpr(ident:Ident, bind:Expr, in:Expr) extends Expr
//  case class PairExpr(left:Expr, right:Expr) extends Expr
//  case class LeftExpr(pair:Expr) extends Expr
//  case class RightExpr(pair:Expr) extends Expr
//  sealed trait ListExpr extends Expr
//  case object Empty extends Expr
//  case class consExpr(tail:Expr) extends Expr
//
//  sealed trait BinOp
//  case object Add extends BinOp
//  case object Sub extends BinOp
//  case object Mul extends BinOp
//  case object Rem extends BinOp
}


