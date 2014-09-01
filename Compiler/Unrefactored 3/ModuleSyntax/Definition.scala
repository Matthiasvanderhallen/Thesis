package ModuleSyntax

import Compiler.{TranslationHelper, Translation, Ident}
import CoreSyntax.Expr
import TypeSystem._

sealed trait Definition{
  def translate(trans:Translation, structure:Structure, signature:Signature):Translation
}

case class FunDefinition(ident:Ident, variables:List[Ident], ascription: FuncType, expression: Expr) extends Definition {
  def translate(trans:Translation, structure:Structure, signature:Signature):Translation = {
    var translation = trans

    val declaration = signature.value.exists{case ValDeclaration(this.ident,_)=>true; case _ =>false}

    val ident = structure.ident.value + "." + this.ident.value

    val retInt = this.ascription.right == TypeSystem.Integer;
    val retTyvar = this.ascription.right.isInstanceOf[VarType]
    val retTypeId = this.ascription.right.getTypeString(structure)
    val retTypeInt = this.ascription.right match{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.qualifiedName(structure))
      case TypeSystem.Integer => TypeSystem.Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => this.ascription.right.intRepresentation
    }

    val argInt = this.ascription.left.map(x => x == TypeSystem.Integer)
    val argTyvar = this.ascription.left.map(x => x.isInstanceOf[VarType])
    val argTypeId = this.ascription.left.map{x => x.getTypeString(structure)}

    val argTypeInt = this.ascription.left.map{
      case s@StructType(ident,_,_) => trans.getOpaqueType(s.qualifiedName(structure))
      case TypeSystem.Integer => TypeSystem.Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("Functions are not allowed as a return type.")}
      case _ => this.ascription.right.intRepresentation
    }

    val argNames = this.variables.map{x=>"%"+x.value}
    val argForInternal = argTypeId.zip(argNames).map{x => x._1 + " "+x._2}.mkString(", ")

    //Internal function output
    var value = s"define private $retTypeId @$ident"+s"_internal($argForInternal){\n"
    value += this.expression.translate(trans, structure, signature, this.ascription.left.zip(argNames), retTypeId)
    value +="}\n\n"

    //External function output
    if(declaration){
      val argumentsAsInt = argTyvar.zip(argNames).map{x => "%int "+x._2+".in" + (if(x._1){", i2 " + x._2 + ".isMask"}else{""})}.mkString(", ")

      value += s"define %int @${ident}_stub($argumentsAsInt) noinline {\n"
      value += s"\t;Switch stack, move parameters\n"

      val argInformation = (this.variables, 1 to this.variables.length,(argTyvar,argTypeId,argTypeInt).zipped.toList).zipped.toList
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


      value += argProcessingCode.mkString("")+ "\tProcess"+ (this.variables.length+1)+":\n"

      value += accessTyvars(this.ascription.left, this.variables,trans,structure,"")

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
                  case TypeSystem.Integer => {
                    //check whether this is an Integer indeed!
                    access += s"\t$tyvarName.tyvar.val = load %tyvar* $tyvarName\n"
                    access += s"\t$tyvarName.tyvar.type = extractvalue %tyvar $tyvarName.tyvar.val, i32 1\n"
                    access += s"\t$tyvarName.tyvar.check = icmp eq %int ${TypeSystem.Integer.intRepresentation}, $tyvarName.tyvar.type\n"
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
              //case TypeSystem.StructType()
              case PairType(left, right) => {
                access += s"\t%ML.typecheck.$prefixrec.pair = inttoptr %int %ML.typecheck.$prefixrec.addr to %pair*\n"
                access += s"\t%ML.typecheck.$prefixrec.pair.val = load %pair* %ML.typecheck.$prefixrec.pair\n"
                access += s"\t%ML.typecheck.$prefixrec.l = extractvalue %pair %ML.typecheck.$prefixrec.pair.val, 0\n" //tyvar*
                access += s"\t%ML.typecheck.$prefixrec.r = extractvalue %pair %ML.typecheck.$prefixrec.pair.val, 0\n" //tyvar*
                access += TranslationHelper.untyvarize(s"%ML.typecheck.$prefixrec.0", s"%ML.typecheck.$prefixrec.l", left, structure)
                access += TranslationHelper.untyvarize(s"%ML.typecheck.$prefixrec.1", s"%ML.typecheck.$prefixrec.r", left, structure)

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
                access += TranslationHelper.untyvarize(s"${prefixrec}.elem", s"%ML.typecheck.$prefixrec.tyvar", inner, structure) //TODO: Wat als hier nu een int in zit?

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
}

case class TypeDefinition (ident:Ident, tyvars:Int, definition: Type) extends Definition {
  def translate(trans:Translation, structure:Structure, signature:Signature):Translation = {
    var translation = trans;

    val typdefident = this.ident

    val declaration = signature.value.find{case OpaqueTypeDeclaration(`typdefident`,_)=>true; case _ => false}

    if(declaration.isEmpty){
      //Transparant type
      translation.types = translation.types.concat("%" + structure.ident + "." + this.ident + " = type {" + this.definition + "}; " + this.definition.intRepresentation + "\n")
    }else{
      //Opaque type
      translation.opaqueTypes = translation.opaqueTypes + 1;
      translation.types = translation.types.concat("%" + structure.ident + "." + this.ident + " = type {" + this.definition + "}; " + translation.opaqueTypes + "\n") //Todo wat met this dependencies
      translation.opaqueTypeMapping = translation.opaqueTypeMapping + ((structure.ident.value + "." + this.ident.value) -> translation.opaqueTypes)
      translation.implMapping = (translation.opaqueTypes, this.definition.intRepresentation) :: translation.implMapping;
      translation.tyvarMapping = translation.tyvarMapping + (translation.opaqueTypes -> this.definition);
    }

    return translation;
  }
}

case class ValDefinition(ident:Ident, ascription: Type, expression: Expr) extends Definition {
  def translate(trans:Translation, structure:Structure, signature:Signature):Translation = {
    var translation = trans

    val valdefident = this.ident

    val declaration = signature.value.find{case ValDeclaration(`valdefident`,_)=>true; case _ => false}

    val ident = structure.ident.value + "." + this.ident.value;
    val returnInt = this.ascription == TypeSystem.Integer;
    val typeId = this.ascription.getTypeString(structure)
    val typeInt = this.ascription match{
      case s@StructType(ident,_,_s) => trans.getOpaqueType(s.qualifiedName(structure))
      case TypeSystem.Integer => TypeSystem.Integer.intRepresentation
      case FuncType(_,_) => {throw new Error("val expression with function type is not allowed.")}
      case _ => this.ascription.intRepresentation
    }

    //Internal function output
    var value = s"define private $typeId @$ident"+s"_internal(){\n"
    value += this.expression.translate(trans, structure, signature, List(), typeId)
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
}