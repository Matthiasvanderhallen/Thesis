package ModuleSyntax

import Compiler.{Ident, Translation}

case class Structure(ident:Ident, signature:Ident, value: List[Definition])
{
  def translate(trans:Translation, signature:Signature):Translation= {

    var translation = trans;

    val types = value.filter{case TypeDefinition(_,_,_) => true; case _ => false}
    val values = value
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
    }

    return translation;
  }

  def createFrame(trans:Translation, structure:Structure, signature:Signature):Translation ={
    var translation = trans;

    translation.frameCounter = translation.frameCounter + 1; //We've created one additional frame.
    return translation
  }
}
