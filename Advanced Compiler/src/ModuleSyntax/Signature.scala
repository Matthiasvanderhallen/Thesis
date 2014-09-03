package ModuleSyntax

import Compiler.Ident

case class Signature(ident:Ident, values: List[Declaration]) {
  def getDeclarationByName(name:String): Declaration = {
    this.values.find{x => x.getIdent == new Ident(name)}.get
  }

  def getIndexByName(name:String):Int = {
    val valdecs = this.values.filter{x => x.isInstanceOf[ValDeclaration]}.sorted.asInstanceOf[List[ValDeclaration]]
    val dec = valdecs.find{x => x.getIdent == new Ident(name)}.get
    valdecs.indexOf(dec)
  }
}
