package Compiler

import ModuleSyntax._
import ProgramSyntax.Program
import TypeSystem.{FuncType, Type}

case class Translation(program:Program, var firstPass:Boolean) {
  var opaqueTypeCounter:Int = 5
  var frameCounter:Int = 0
  var types:String = ""
  var entryPoints:String = ""
  var values:String = ""
  var initialize:String = ""
  var opaqueTypeMapping:Map[String,Int] = Map().+("%int" -> 1).+("%tyvar" -> 2).+("%pair" -> 3).+("%array" -> 4)
  var implMapping:Map[Int,Int] = Map()
  var tyvarMapping:Map[Int,Type] = Map()

  def structToFrame(structureDef:StructureDefinition):Int = {
    structureDef match {
      case Structure(_, _, _) => program.structures.filterNot { case _ => true}.sorted.indexOf(structureDef) //TODO
      case Functor(_, _, _, _, _) => throw new IllegalArgumentException("Functors have no associated frame, only functor applications have frames")
      case _ => throw new IllegalArgumentException("")
    }
  }

  override def toString:String = {
    val defaultTypes =
      "%int = type i64 ; 1\n" +
      "%tyvar = type {%int, %int} ; 2\n" +
      "%pair = type {%tyvar*, %tyvar*} ; 3\n" +
      "%array = type {%int, [0 x %tyvar*]} ; 4\n" +
      "%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}\n" +
      "%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*}\n" +
      "%typedef = type {[0 x i8]*, %int, %int, %type*, i1}\n" +
      "%valuedef = type {[0 x i8]*, %type*}\n" +
      "%type = type {%int, %int, %int}"

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

    val initial = "define void @initialize(){\n" +
      initialize +"}\n"

    //TODO: hier frame opzoeken in lijst, en laden...
    val callStructureValue = "define %int @callStructureValue(%int %frame, %int %value, i8* %args){\n" +
      "\tret %int 0\n" +
      "}\n"
    defaultTypes +";Non-standard Types\n" +types + "\n"+ declarations + "\n" + entryPoints + "\n" + values + main + "\n" + initial + "\n" +callStructureValue
  }

  def getImplTypeInt(search:Int):Int = {
    val result = implMapping.get(search).get //implMapping.filter{case (`search`, _) => true; case _ => false}.head._2
    if(result > 5)
      return getImplTypeInt(result)
    else
      return result
  }

  def getOpaqueType(str:String):Int = {
    opaqueTypeMapping.get(str).get
  }

  def getImplType(search:Int):Type = {
    tyvarMapping.get(search).get
  }

  def getSignatureByName(name:String) : Signature = {
    val ident = new Ident(name);
    program.signatures.find{case Signature(`ident`, _) => true; case _ => false}.get
  }

  def getFuncType(structureDef:StructureDefinition, search:Ident):FuncType = {
    if(search.value.contains(".")){
      val defStructIdent = new Ident(search.value.split("""\.""")(0))
      val valueIdent = new Ident(search.value.split("""\.""")(1))
      if(defStructIdent == structureDef.ident)
        return structureDef.values.find{case FunDefinition(`search`,_,_,_) => true ;case _ => false}.get.asInstanceOf[FunDefinition].ascription

      val defStruct = program.structures.find{case Structure(`defStructIdent`,_,_) => true ; case _ => false}.get
      val sigId = defStruct.signature
      val defSignature = program.signatures.find{case Signature(`sigId`,_) => true; case _ => false}.get
      return defSignature.values.find{case ValDeclaration(`valueIdent`,_) => true; case _ => false}.get.asInstanceOf[ValDeclaration].ascription.asInstanceOf[FuncType]
    }else{
      return structureDef.values.find{case FunDefinition(`search`,_,_,_) => true ;case _ => false}.get.asInstanceOf[FunDefinition].ascription
    }
  }

  def getValType(structureDef:StructureDefinition, search:Ident) : Type = {
    if(search.value.contains(".")){
      val defStructIdent = new Ident(search.value.split("""\.""")(0))
      val valueIdent = new Ident(search.value.split("""\.""")(1))
      if(defStructIdent == structureDef.ident)
        return structureDef.values.find{case ValDefinition(`search`,_,_) => true ; case _ => false}.get.asInstanceOf[ValDefinition].ascription

      val defStruct = program.structures.find{case Structure(`defStructIdent`,_,_) => true; case _ => false}.get
      val sigId = defStruct.signature
      val defSignature = program.signatures.find{case Signature(`sigId`,_) => true; case _ => false}.get
      return defSignature.values.find{case ValDeclaration(`valueIdent`,_) => true; case _ => false}.get.asInstanceOf[ValDeclaration].ascription
    }else{
      return structureDef.values.find{case ValDefinition(`search`,_,_) => true ; case _ => false}.get.asInstanceOf[ValDefinition].ascription
    }
  }

  def getStructureDefinitionByName(name:String) : StructureDefinition = {
    val ident = new Ident(name)
    program.structures.find(x => x.ident == `ident`).get
  }
}
