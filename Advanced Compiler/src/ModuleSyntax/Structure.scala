package ModuleSyntax

import Compiler.{Ident, Translation}

case class Structure(ident:Ident, signature:Ident, values: List[Definition])
{
  def translate(trans:Translation, signature:Signature):Translation= {

    var translation = trans;

    val types = this.values.filter{case TypeDefinition(_,_,_) => true; case _ => false}
    val values = this.values
      .filter{case TypeDefinition(_,_,_) => false; case _ => true}
      .sortBy{case TypeDefinition(ident,_,_) => ident.value;
              case ValDefinition(ident,_,_) => ident.value;
              case FunDefinition(ident,_,_,_) => ident.value
             }

    if(translation.firstPass){
      for(typedef <- types){
        translation = typedef.translate(translation,this, signature)
      }
    } else {
      for(valdef <- values){
        translation = valdef.translate(translation, this, signature)
      }
      translation = createFrame(translation, this, signature)
    }

    return translation;
  }

  //Create a record of style:
  //%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
  def createFrame(trans:Translation, structure:Structure, signature:Signature):Translation ={
    var translation = trans;

    val fname = s"%frame.${this.ident.value}"
    val identifier = s"$fname.identifier"
    val argFrame = s"$fname.argframe"
    val securityBit = s"$fname.isSecure"
    val trimmingMap = s"$fname.trimmingMap"
    val valueMap = s"$fname.valueMap"
    val opaqueTypeMap = s"$fname.opaqueTypeMap"
    val metaframe = s"$fname.metaframe"

    var body = ""
    body += s"\t$fname.addr = call i8* @malloc(%int ${8*6+1})\n"
    body += s"\t$fname = bitcast i8* $fname.addr to %frame*\n"

    //identifier
    //body += s"$identifier.addr"
    body += s"\t$identifier = insertvalue %frame undef, [0 x i8]* null, 0 \n"

    //argFrame
    body += s"\t$argFrame = insertvalue %frame $identifier, %frame* null, 1 \n"

    //securityBit
    body += s"\t$securityBit = insertvalue %frame $argFrame, i1 0, 2 \n"

    //trimmingMap
    body += s"\t$trimmingMap = insertvalue %frame $securityBit, {%int, [0 x %int]}* null, 3 \n"

    //valueMap
    val nbOfValues = signature.values.filter{case ValDeclaration(_,_) => true; case _ => false;}.length
    body += s"\t$valueMap.addr = call i8* @malloc(%int ${8*nbOfValues})\n"
    body += s"\t$valueMap.ptr = bitcast i8* $valueMap.addr to {%int, [0 x %int (%frame*, i8*)*]}* \n"
    body += s"\t$valueMap.length.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, i32 0, i32 0 \n"
    body += s"\tstore %int $nbOfValues, %int* $valueMap.length.ptr \n"
    var index = 0;
    for(value:Declaration <- signature.values.filter{case ValDeclaration(_,_) => true; case _ => false;}){
      body += s"\t$valueMap.elem.$index.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, i32 0, i32 1, %int $index \n"
      body += s"\tstore %int (%frame*, i8*)* @${value.getIdent.internalize(this)}, %int (%frame*, i8*)** \n"
      index = index + 1
    }
    body += s"\t$valueMap = insertvalue %frame $trimmingMap, {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, 4\n"

    //opaqueTypeMap
    body += s"\t$opaqueTypeMap = insertvalue %frame $valueMap, {%int, [0 x %int]}* null, 5 \n"

    //metaframe

    body += s"\t$metaframe = insertvalue %frame $opaqueTypeMap, %metaframe* null, 6\n"

    body += s"\tstore %frame $metaframe, %frame* $fname"

    //println(body)

    translation.initialize = translation.initialize + "\n" + body
    translation.frameCounter = translation.frameCounter + 1; //We've created one additional frame.
    return translation
  }

  def createMetaFrame(trans:Translation, structure:Structure, signature:Signature):Translation = {
    var translation = trans

    return translation
  }
}
