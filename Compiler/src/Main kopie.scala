import com.sun.tools.javac.code.Type.TypeVar
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

case class Translation(var types:String, var entryPoints:String, var values:String, var opaqueTypes:Int, var opaqueTypeMapping:Map[String,Int], var implMapping:List[(Int,Int)], var tyvarMapping:Map[Int,Type]) {
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
                        new FunDefinition(new Ident("getLeft"), List(new Ident("pair")), new FuncType(List(new StructType(new Ident("pair"), 1, List(new VarType(new Ident("a"))))), new VarType(new Ident("a"))), ConstExpr(3)),
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
    var translation = new Translation("", "", "", 5, Map(), List(), Map());

    val structures = program.structures.sortBy(struct => struct.ident.value)

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

    for(typedef <- types){
      translation = translateDefinition(translation, typedef, structure, signature)
    }

    for(valdef <- values){
      translation = translateDefinition(translation, valdef, structure, signature)
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
//    val implementation = typedef.definition match{
//      case Integer => "%int"
//      case PairType(_,_) => "%pair"
//      case ListType(_) => "%array"
//      case _ => ""
//    }

    val declaration = signature.value.find{case OpaqueTypeDeclaration(typedef.ident,_)=>true; case _ => false}

    if(declaration.isEmpty){
      //Transparant type
      translation.types = translation.types.concat("%" + structure.ident + "." + typedef.ident + " = type {" + typedef.definition + "}; " + typedef.definition.intRepresentation + "\n")
    }else{
      //Opaque type
      translation.opaqueTypes = translation.opaqueTypes + 1;
      translation.types = translation.types.concat("%" + structure.ident + "." + typedef.ident + " = type {" + typedef.definition + "}; " + translation.opaqueTypes + "\n")
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
    //value += translateBody(valdef, structure, signature)
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

      value += createTyvarChecks(fundef.ascription.left, fundef.variables, structure, signature, trans); //Arguments and their names

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


  def createTyvarChecks(arguments: List[Type], variables:List[Ident], structure:Structure, signature:Signature, trans:Translation):String = {
    /*var identMap = Map[Ident, List[List[Int]]]() //This mapping contains ident vars from the argument types, and maps them to locations
    var instantiationsMap = List[(List[Int], Type)]() // This map contains instantiated tyvars of the parametric type.

    for ((argType, loc) <- arguments.zip(0 to arguments.length-1)) {
      argType match {
        case VarType(ident) => {
          identMap = identMap.updated(ident, List(loc) :: identMap.getOrElse(ident, List()))
        }
        case x: StructType => {
          val pair = getVarTypeInformation(List(loc), x, identMap)
          identMap = pair._1;
          instantiationsMap.:+((List(loc), x)).++(pair._2);
        }
        case _ => {}
      }
    }

    if(identMap.nonEmpty)
      println(identMap + "\n " + instantiationsMap)

    */
    return accessTyvars(arguments, variables,trans,structure,signature,"",Map[Ident, String]())._1 + "\n"
    /*
    var checks = ""
    var tyvarIndex = 0;
    for (ident<-identMap.keys)
    {
      val occurences = identMap.getOrElse(ident,List())
      if(occurences.length > 1)
      {
        val reprLoc = identMap.get(ident).get.head
        getTyvarRepresentingLoc(arguments, variables, reprLoc, tyvarIndex);
      }

    }*/
  }

  def accessTyvars(arguments:List[Type], variables:List[Ident], trans:Translation, structure:Structure, signature:Signature, prefix:String, rep:Map[Ident, String]):(String,Map[Ident, String]) = {
    var access = ""
    var representatives = rep;
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
        case StructType(_,0,_) => {}
        case s@StructType(ident,amount,types) => {
//          if(prefix != "")
//          {
//            //check if type correct
//          }
//          //if(prefix == ""){
//            //type reeds gechecked.
            val argName = variables(loc).value

            val structTypeString = s.internalize(structure)
            val structTypeInt = trans.getOpaqueType(structTypeString)
            val implTypInt = trans.getImplTypeInt(structTypeInt)
            val implType = trans.getImplType(structTypeInt)//;.getVarTypeStr(trans);

            var packed = implType;
            var prefixrec = prefix;
            var locrec = loc;

            recFunc = {
              packed match{
                case VarType(ident) => {
                  val tyvarName = s"%ML.typecheck.$prefix$loc"

                  access += s"\t$tyvarName = bitcast i8* %$argName.tyvar.addr to %tyvar*\n"
                }
                //case StructType()
                case PairType(left, right) => {
                  access += s"\t%ML.typecheck.$prefixrec$locrec.pair = inttoptr %int $argName.addr to %pair*\n"
                  access += s"\t%ML.typecheck.$prefixrec$locrec.0.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 0 ; tyvar*\n"
                  access += s"\t%ML.typecheck.$prefixrec$locrec.1.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 1 ; tyvar*\n"

                  recFunc
                  /*var typesL = List[Type]()
                  for(i<-left.getVarTypes(trans)){
                    //typesL = typesL :+ types(i)
                  }*/



                  //Klopt nog niet: left en right moeten iets anders zijn.
                  //val rec = accessTyvars(List(left,right),List[Ident](), trans, structure, signature, s"$prefix$loc.", representatives)
                  //access +=rec._1;
                  //representatives = rec._2;

                  /*var typesR = List[Type]()
                  for(i<-right.getVarTypes(trans)){
                    typesR = typesR :+ types(i)
                  }*/
                  //val recR = accessTyvars(List(right),List[Ident](), trans, structure, signature, s"$prefix$loc.", representatives)
                  //access +=recR._1;
                  //representatives = recR._2;


                }
                case ListType(inner) => {
                    access += s"\t%ML.typecheck.$prefix$loc.array.ptr = inttoptr %int $argName.addr to %array*\n"
                    access += s"\t%ML.typecheck.$prefix$loc.array = load %array* %ML.typecheck.$prefix$loc.array.ptr\n"
                    access += s"\t%ML.typecheck.$prefix$loc.array.length = extractvalue %array %ML.typecheck.$prefix$loc.array, 0\n"
                    access += s"\t%ML.typecheck.$prefix$loc.check = icmp eq %int 0, %ML.typecheck.$prefix$loc.array.length\n"
                    access += s"\tbr %ML.typecheck.$prefix$loc.check, label %Skip$prefix$loc, label %Continue$prefix$loc\n\n"

                    access += s"\tContinue$prefix$loc:\n"
                    access += s"\t%ML.typecheck.$prefix$loc.0.addr = getelementptr %array %ML.typecheck.$prefix$loc.array.ptr, i32 0, %int 1"
                    val rec = accessTyvars(List(inner),List[Ident](), trans, structure, signature, s"$prefix$loc.", representatives)
                    access +=rec._1;
                    representatives = rec._2;
                    access += s"br label %Skip$prefix$loc\n\n"
                    access += s"Skip$prefix$loc:\n"

                    //access += s"\t%ML.typecheck.$prefix$loc.0.addr = getelementptr %pair* %ML.typecheck.$prefix$loc.pair, i32 0, %int 0 ; tyvar*\n"
                }
                case _ => {throw new UnsupportedOperationException()}
              }
            }
        }
        case _ => {} //Only Integers, because no pairs or arrays can be argument types! :)
      }
    }
    println(access)
    return (access,representatives)
  }

  def getTyvarRepresentingLoc(arguments: List[Type], variables:List[Ident], loc:List[Int], ind:Int):String = {
    var code = ""
    println(variables)
    if(loc.length == 1){
      val name = variables(loc.head).value
      code += s"\t%ML.tyvar$ind = bitcast i8* %$name.tyvar.addr to %tyvar*"
    }else{

      code += s"\t"
    }

    println(code);

    return code
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
    //value += translateBody(valdef, structure, signature)
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

  def getVarTypeInformation(prefix:List[Int], argType:StructType, idents:Map[Ident,List[List[Int]]]):(Map[Ident,List[List[Int]]],List[(List[Int], Type)]) = {
    var identMap = idents;
    var instantiationsMap = List[(List[Int], Type)]() // This map contains instantiated tyvars of the parametric type.

    for((argType, loc)<-argType.tyvars.zip(0 to argType.nrTyvars-1)){
      argType match{
        case VarType(ident) => {identMap = identMap.updated(ident, (prefix.:+(loc)) :: identMap.getOrElse(ident, List()))}
        case x:StructType => { val pair = getVarTypeInformation((prefix.:+(loc)),x,identMap)
          identMap = pair._1;
          instantiationsMap.+:((prefix.:+(loc), x)).++(pair._2);
        }
        case _ => {}
      }
    }

    return (identMap, instantiationsMap)
  }
}


