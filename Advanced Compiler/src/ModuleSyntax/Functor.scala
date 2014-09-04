package ModuleSyntax

import Compiler.{Translation, Ident}

case class Functor(ident:Ident, argIdent:Ident, argSignature:Ident, signature:Ident, values: List[Definition]) extends StructureDefinition
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
        translation = valdef.translate(translation, this, signature) //TODO: Adding functors
      }
      //translation = createFrame(translation, signature)
      translation = createFunctorFrameGenerator(trans:Translation, signature)
    }

    return translation;
  }

  //Create a record of style:
  //%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
  def createFrame(trans:Translation, signature:Signature):Translation ={
    throw new UnsupportedOperationException("Functor definitions can't create frames, only applications do.")
  }

  def createFunctorFrameGenerator(trans:Translation, signature:Signature): Translation ={
    var translation = trans

    val fnameExt = s"%frame.${this.ident.value}.ext"
    val fnameInt = s"%frame.${this.ident.value}.int"
    var value = ""

    value += s"define void @${ident}(%int %frame, i1 %ext, i8* %vargs){\n"
    value += s"\tcall void @initialize()\n"
    value += s"\t%frIndex = load %int* @frameCounter\n"
    value += s"\t%flist.ptr = load [100 x %frame*]** @flist\n"
    value += s"\tbr i1 %ext, label %External, label %Secure\n"
    value += s"\tExternal:\n"
    value += createFrameExternal(translation, signature)
    //value += s"\tinsertvalue %[100 x %frame*]* %flist.ptr, %frame* $fnameExt, %frIndex\n"
    value += s"\t%ext.ptr = getelementptr [100 x %frame*]* %flist.ptr, i32 0, %int %frIndex\n"
    value += s"\tstore %frame* $fnameExt, %frame** %ext.ptr\n"
    value += s"\tbr label %Return\n"
    value += s"\tSecure:\n"
    value += createFrameInternal(translation, signature)
    value += s"\t%int.ptr = getelementptr [100 x %frame*]* %flist.ptr, i32 0, %int %frIndex\n"
    value += s"\tstore %frame* $fnameInt, %frame** %int.ptr\n"
    value += s"\tbr label %Return\n"
    value += s"\tReturn:\n"
    value += s"\t%frIndexNew = add %int 1, %frIndex\n"
    value += s"\tstore %int %frIndexNew, %int* @frameCounter\n"
    value += "\tret void\n"
    value += "}\n"

    translation.values = translation.values.concat(s"\n$value\n")
    return translation
  }

  def createFrameExternal(trans:Translation, signature:Signature):String = {
    val fname = s"%frame.${this.ident.value}.ext"
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
    body += s"\t%argFrame.ext.ptr = inttoptr %int %frame to %frame*"
    body += s"\t$argFrame = insertvalue %frame $identifier, %frame* %argFrame.ext.ptr, 1 \n"

    //securityBit
    body += s"\t$securityBit = insertvalue %frame $argFrame, i1 0, 2 \n"

    //trimmingMap
    val nbOfValues = signature.values.count({x => x.isInstanceOf[ValDeclaration]})
    val tempTrimmingMap = "%tempTrimmingMap.ext"
    body += s"\t$tempTrimmingMap.addr = call i8* @malloc(%int ${8*(nbOfValues+1)})\n"
    body += s"\t$tempTrimmingMap.ptr = bitcast i8* $tempTrimmingMap.addr to {%int, [${nbOfValues} x %int]}*\n"
    body += s"\t%argTrimmingMap.ext.ptr = bitcast i8* %vargs to {%int, [${nbOfValues} x %int]}*\n"
    body += s"\t%argTrimmingMap.ext.val = load {%int, [${nbOfValues} x %int]}* %argTrimmingMap.ext.ptr\n"
    body += s"\tstore {%int, [${nbOfValues} x %int]} %argTrimmingMap.ext.val, {%int, [${nbOfValues} x %int]}* $tempTrimmingMap.ptr\n"
    body += s"\t$tempTrimmingMap.ptr2 = bitcast {%int, [${nbOfValues} x %int]}* $tempTrimmingMap.ptr to {%int, [0 x %int]}*"
    body += s"\t$trimmingMap = insertvalue %frame $securityBit, {%int, [0 x %int]}* $tempTrimmingMap.ptr2, 3 \n"

    //valueMap
    //val nbOfValues = signature.values.count{case ValDeclaration(_,_) => true; case _ => false;}
    body += s"\t$valueMap.addr = call i8* @malloc(%int ${8*nbOfValues*2+8})\n"
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
    body += s"\t%nbOfOpaqueTypes.0 = load %int* @opaqueTypeCounter\n"
    var nbOfOpaqueTypes = signature.values.sorted.count(x => x.isInstanceOf[OpaqueTypeDeclaration])
    body += s"\t$opaqueTypeMap.addr = call i8* @malloc(%int ${8*nbOfOpaqueTypes})\n"
    body += s"\t$opaqueTypeMap.ptr = bitcast i8* $opaqueTypeMap.addr to {%int, [0 x %int]}* \n"
    body += s"\t$opaqueTypeMap.length.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 0 \n"
    body += s"\tstore %int ${nbOfOpaqueTypes}, %int* $opaqueTypeMap.length.ptr \n"

    index = 0
    for(declaration:Declaration <- signature.values.sorted){
      declaration match{
        case OpaqueTypeDeclaration(ident, tyvars) => {
          body += s"\t$opaqueTypeMap.$ident.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 1, %int $index \n"
          body += s"\tstore %int %nbOfOpaqueTypes.$index, %int* $opaqueTypeMap.$ident.ptr\n"
          index = index + 1
          body += s"\t%nbOfOpaqueTypes.$index = add %int 1, %nbOfOpaqueTypes.${index-1}\n"
        }
        case _ => {}
      }
    }
    body += s"\t$opaqueTypeMap = insertvalue %frame $valueMap, {%int, [0 x %int]}* $opaqueTypeMap.ptr, 5 \n"
    body += s"\tstore %int %nbOfOpaqueTypes.$index, %int* @opaqueTypeCounter\n"

    //metaframe

    body += s"\t$metaframe = insertvalue %frame $opaqueTypeMap, %metaframe* null, 6\n"

    body += s"\tstore %frame $metaframe, %frame* $fname\n"

    return body;
  }

  def createFrameInternal(trans:Translation, signature:Signature):String = {
    val fname = s"%frame.${this.ident.value}.int"
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
    body += s"\t%flist = load [100 x %frame*]** @flist\n"
    body += s"\t%argFrame.int.ptr.ptr = getelementptr [100 x %frame*]* %flist, i32 0, %int %frame\n"
    body += s"\t%argFrame.int.ptr = load %frame** %argFrame.int.ptr.ptr\n"
    body += s"\t$argFrame = insertvalue %frame $identifier, %frame* %argFrame.int.ptr, 1 \n"

    //securityBit
    body += s"\t$securityBit = insertvalue %frame $argFrame, i1 0, 2 \n"

    //trimmingMap
    val nbOfValues = signature.values.count({x => x.isInstanceOf[ValDeclaration]})
    val tempTrimmingMap = "%tempTrimmingMap.int"
    body += s"\t$tempTrimmingMap.addr = call i8* @malloc(%int ${8*(nbOfValues+1)})\n"
    body += s"\t$tempTrimmingMap.ptr = bitcast i8* $tempTrimmingMap.addr to {%int, [${nbOfValues} x %int]}*\n"
    body += s"\t%argTrimmingMap.ptr = bitcast i8* %vargs to {%int, [${nbOfValues} x %int]}*\n"
    body += s"\t%argTrimmingMap.val = load {%int, [${nbOfValues} x %int]}* %argTrimmingMap.ptr\n"
    body += s"\tstore {%int, [${nbOfValues} x %int]} %argTrimmingMap.val, {%int, [${nbOfValues} x %int]}* $tempTrimmingMap.ptr\n"
    body += s"\t$tempTrimmingMap.ptr2 = bitcast {%int, [${nbOfValues} x %int]}* $tempTrimmingMap.ptr to {%int, [0 x %int]}*"
    body += s"\t$trimmingMap = insertvalue %frame $securityBit, {%int, [0 x %int]}* $tempTrimmingMap.ptr2, 3 \n"

    //valueMap
    //val nbOfValues = signature.values.count{case ValDeclaration(_,_) => true; case _ => false;}
    body += s"\t$valueMap.addr = call i8* @malloc(%int ${8*nbOfValues*2+8})\n"
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
    body += s"\t%nbOfOpaqueTypes.int.0 = load %int* @opaqueTypeCounter\n"
    var nbOfOpaqueTypes = signature.values.sorted.count(x => x.isInstanceOf[OpaqueTypeDeclaration])
    body += s"\t$opaqueTypeMap.addr = call i8* @malloc(%int ${8*nbOfOpaqueTypes})\n"
    body += s"\t$opaqueTypeMap.ptr = bitcast i8* $opaqueTypeMap.addr to {%int, [0 x %int]}* \n"
    body += s"\t$opaqueTypeMap.length.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 0 \n"
    body += s"\tstore %int ${nbOfOpaqueTypes}, %int* $opaqueTypeMap.length.ptr \n"
    index = 0
    for(declaration:Declaration <- signature.values.sorted){
      declaration match{
        case OpaqueTypeDeclaration(ident, tyvars) => {
          body += s"\t$opaqueTypeMap.$ident.ptr = getelementptr {%int, [0 x %int]}* $opaqueTypeMap.ptr, i32 0, i32 1, %int $index \n"
          body += s"\tstore %int %nbOfOpaqueTypes.int.$index, %int* $opaqueTypeMap.$ident.ptr\n"
          index = index + 1
          body += s"\t%nbOfOpaqueTypes.int.$index = add %int 1, %nbOfOpaqueTypes.int.${index-1}\n"
        }
        case _ => {}
      }
    }
    body += s"\t$opaqueTypeMap = insertvalue %frame $valueMap, {%int, [0 x %int]}* $opaqueTypeMap.ptr, 5 \n"
    body += s"\tstore %int %nbOfOpaqueTypes.int.$index, %int* @opaqueTypeCounter\n"

    //metaframe

    body += s"\t$metaframe = insertvalue %frame $opaqueTypeMap, %metaframe* null, 6\n"

    body += s"\tstore %frame $metaframe, %frame* $fname\n"

    return body;
  }

  def createMetaFrame(trans:Translation, signature:Signature):Translation = {
    var translation = trans

    return translation
  }
}
