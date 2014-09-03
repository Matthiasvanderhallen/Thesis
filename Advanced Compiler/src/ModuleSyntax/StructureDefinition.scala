package ModuleSyntax

import Compiler.{Translation, Ident}

trait StructureDefinition extends Ordered[StructureDefinition]{
  val ident:Ident
  val values:List[Definition]
  val signature:Ident
  def translate(trans:Translation, signature:Signature):Translation
  //def createFrame(trans:Translation, signature:Signature):Translation
  //def createMetaFrame(trans:Translation, signature:Signature):Translation
  def compare(a:StructureDefinition) = ident.compareTo(a.ident)
  def getTypeIndex(ident:Ident):Int ={
    this.values.filter{case TypeDefinition(_,_,_) => true; case _ => false}.map{x => x.ident}.sorted.indexOf(ident)
  }
}
