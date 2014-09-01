package ModuleSyntax

import TypeSystem.Type
import Compiler.Ident

sealed trait Declaration {
  def getIdent : Ident
}
sealed trait TypeDeclaration extends Declaration
case class TransparentTypeDeclaration(ident:Ident, tyvars:Int, definition: Type) extends TypeDeclaration {
  override def getIdent: Ident = ident
}
case class OpaqueTypeDeclaration(ident:Ident, tyvars:Int) extends TypeDeclaration {
  override def getIdent: Ident = ident
}
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration {
  override def getIdent: Ident = ident
}