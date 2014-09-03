package ModuleSyntax

import Compiler.{Ident, Translation}

case class Structure(ident:Ident, signature:Ident, values: List[Definition]) extends StructureDefinition
{
  def translate(trans:Translation, signature:Signature):Translation= {

    var translation = trans;

    val types = this.values.filter{case TypeDefinition(_,_,_) => true; case _ => false}
    val values = this.values
      .filter{case TypeDefinition(_,_,_) => false; case _ => true}
      .sorted

    if(translation.firstPass){
      for(typedef <- types){
        translation = typedef.translate(translation,this, signature)
      }
    } else {
      for(valdef <- values){
        translation = valdef.translate(translation, this, signature)
      }
      translation = createFrame(translation, signature)
    }

    return translation;
  }

  //Create a record of style:
  //%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
  def createFrame(trans:Translation, signature:Signature):Translation ={
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
    val nbOfValues = signature.values.count{case ValDeclaration(_,_) => true; case _ => false;}
    body += s"\t$valueMap.addr = call i8* @malloc(%int ${8*nbOfValues*2})\n"
    body += s"\t$valueMap.ptr = bitcast i8* $valueMap.addr to {%int, [0 x %int (%frame*, i8*)*]}* \n"
    body += s"\t$valueMap.length.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, i32 0, i32 0 \n"
    body += s"\tstore %int ${nbOfValues*2}, %int* $valueMap.length.ptr \n"
    var index = 0;
    for(declaration:Declaration <- signature.values){
      declaration match{
        case value@ValDeclaration(_,_) => {
          //Stub
          body += s"\t$valueMap.elem.$index.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, i32 0, i32 1, %int $index \n"
          body += s"\tstore %int (%frame*, i8*)* @${value.getIdent.internalize(this)}_stub, %int (%frame*, i8*)** $valueMap.elem.$index.ptr\n"
          index = index + 1

          //Internal
          body += s"\t$valueMap.elem.$index.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, i32 0, i32 1, %int $index \n"
          //body += s"\t$valueMap.elem.$index.val.ptr = bitcast ${value.getTypeString(this)}* @${value.getIdent.internalize(this)}_internal to %int (%frame*, i8*)* \n"
          body += s"\tstore %int (%frame*, i8*)* @${value.getIdent.internalize(this)}_intstub, %int (%frame*, i8*)** $valueMap.elem.$index.ptr\n"
          index = index + 1
        }
        case _ => {

        }
      }
    }
    body += s"\t$valueMap = insertvalue %frame $trimmingMap, {%int, [0 x %int (%frame*, i8*)*]}* $valueMap.ptr, 4\n"

    //opaqueTypeMap
    val nbOfOpaqueTypes = signature.values.count{case OpaqueTypeDeclaration(_,_) => true; case _ => false}
    body += s"\t$opaqueTypeMap.addr = call i8* @malloc(%int ${8*nbOfOpaqueTypes})\n"
    body += s"\t$opaqueTypeMap.ptr = bitcast i8* $valueMap.addr to {%int, [0 x %int]}* \n"
    body += s"\t$opaqueTypeMap.length.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 0 \n"
    body += s"\tstore %int ${nbOfOpaqueTypes}, %int* $opaqueTypeMap.length.ptr \n"

    index = 0
    for(declaration:Declaration <- signature.values.sorted){
      declaration match{
        case OpaqueTypeDeclaration(ident, tyvars) => {
          body += s"\t$opaqueTypeMap.$ident.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 1, %int $index \n"
          body += s"\tstore %int ${translation.getOpaqueType(ident.internalize(this))}, %int* $opaqueTypeMap.$ident.ptr"
          index = index + 1
        }
        case _ => {}
      }
    }
    body += s"\t$opaqueTypeMap = insertvalue %frame $valueMap, {%int, [0 x %int]}* $opaqueTypeMap.ptr, 5 \n"

    //metaframe

    body += s"\t$metaframe = insertvalue %frame $opaqueTypeMap, %metaframe* null, 6\n"

    body += s"\tstore %frame $metaframe, %frame* $fname\n"

    body += s"\tret void\n"

    //println(body)

    translation.initialize = translation.initialize + "\n" + body
    translation.frameCounter = translation.frameCounter + 1; //We've created one additional frame.
    return translation
  }

  def createMetaFrame(trans:Translation, signature:Signature):Translation = {
    var translation = trans

    return translation
  }
}
