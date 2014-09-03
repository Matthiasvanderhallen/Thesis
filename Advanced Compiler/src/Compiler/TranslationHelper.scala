package Compiler

import ModuleSyntax.{StructureDefinition, Structure}
import TypeSystem.{VarType, ListType, Type}

object TranslationHelper {
  def getAsArray(desiredName:String, currentName:String, currentType:Type, structureDef:StructureDefinition):String = {
    var body = ""
    if(!currentType.isInstanceOf[ListType]){ //If the tail is of a different type than List type -> LLVM code to convert
      if(currentType.isInstanceOf[VarType]){  //If it is a tyvar, get the
        body +=s"\t${desiredName}.tyvar.val = load %tyvar* $currentName\n"
        body +=s"\t${desiredName}.addr = extractvalue %tyvar ${desiredName}.tyvar.val, 0 ; extract address from tyvar\n"
        body +=s"\t${desiredName} = inttoptr %int ${desiredName}.array.addr to %array* ; cast address to array*\n" // bitcast -> inttoptr
      }else{
        body +=s"\t${desiredName} = bitcast ${currentType.getTypeString(structureDef)} $currentName to %array*\n" //Added *, the original type can never be a non-pointer type
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
  def tyvarize(desiredName:String, valueToTyvarize:String, currentType:Type, structureDef:StructureDefinition):String = {
    var body = ""
    body += s"\t${desiredName}.addr = call i8* @malloc(%int 16)\n"
    body += s"\t${desiredName} = bitcast i8* ${desiredName}.addr to %tyvar*\n"
    if(currentType != TypeSystem.Integer){
      body += s"\t${desiredName}.int = ptrtoint ${currentType.getTypeString(structureDef)} ${valueToTyvarize} to %int; cast ptr to int\n" //added *
      body += s"\t${desiredName}.0 = insertvalue %tyvar undef, %int ${desiredName}.int, 0 ; create tyvar step 1\n "
    }else{
      body += s"\t${desiredName}.0 = insertvalue %tyvar undef, %int ${valueToTyvarize}, 0 ; create tyvar step 1\n "
    }
    body += s"\t${desiredName}.1 = insertvalue %tyvar ${desiredName}.0, %int ${currentType.intRepresentation}, 1 ; create tyvar step 2\n "
    body += s"\tstore %tyvar ${desiredName}.1, %tyvar* ${desiredName} ; Store the tyvar \n" //save

    return body;
  }

  def untyvarize(desiredName:String, tyvarPtr:String, annot:Type, structureDef:StructureDefinition):String = {
    var body = ""

    if(annot.isInstanceOf[VarType]){
      body += s"\t${desiredName}.addr = ptrtoint %tyvar* ${tyvarPtr} to %int\n"
      body += s"\t${desiredName} = inttoptr %int ${desiredName}.addr to %tyvar*\n"
      body += s"\t${desiredName}.val = load %tyvar* ${desiredName} \n"
    } else {
      body += s"\t${desiredName}.tyvar = load %tyvar* ${tyvarPtr}\n"

      if (annot == TypeSystem.Integer) {
        body += s"\t${desiredName} = extractvalue %tyvar ${desiredName}.tyvar, 0\n"
        body += s"\t${desiredName}.val = extractvalue %tyvar ${desiredName}.tyvar, 0\n"
      } else {
        body += s"\t${desiredName}.addr = extractvalue %tyvar ${desiredName}.tyvar, 0\n ; int"
        body += s"\t${desiredName} = inttoptr %int ${desiredName}.addr to ${annot.getTypeString(structureDef)}\n"
        body += s"\t${desiredName}.val = load ${annot.getTypeString(structureDef)} ${desiredName}\n" //Load the value.
      }
    }

    return body
  }
}
