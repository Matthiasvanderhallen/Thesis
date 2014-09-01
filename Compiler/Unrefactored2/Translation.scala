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