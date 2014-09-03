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
    }

    return translation;
  }

  //Create a record of style:
  //%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
  def createFrame(trans:Translation, signature:Signature):Translation ={
    throw new UnsupportedOperationException("Functor definitions can't create frames, only applications do.")
  }

  def createFunctorFrameGenerator(trans:Translation): Translation ={
    var translation = trans

    return translation
  }

  def createMetaFrame(trans:Translation, signature:Signature):Translation = {
    var translation = trans

    return translation
  }
}
