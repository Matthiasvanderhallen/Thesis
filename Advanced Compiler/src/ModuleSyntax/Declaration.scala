package ModuleSyntax

import TypeSystem.{FuncType, Type}
import Compiler.Ident

sealed trait Declaration extends Ordered[Declaration] {
  def getIdent : Ident
  def compare(other:Declaration) = getIdent.compareTo(other.getIdent)
}
sealed trait TypeDeclaration extends Declaration
case class TransparentTypeDeclaration(ident:Ident, tyvars:Int, definition: Type) extends TypeDeclaration {
  override def getIdent: Ident = ident
}
case class OpaqueTypeDeclaration(ident:Ident, tyvars:Int) extends TypeDeclaration {
  override def getIdent: Ident = ident
}
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration {
  def getTypeString(structureDef:StructureDefinition): String ={
    ascription match{
      case FuncType(left, right) => {
        val argTypeId = left.map { x => x.getTypeString(structureDef)}
        val retTypeId = right.getTypeString(structureDef)

        return s"$retTypeId (%frame*, ${argTypeId.mkString(", ")})"
      }
      case _ => {
        val retTypeId = this.ascription.getTypeString(structureDef)

        return s"$retTypeId (%frame*)"
      }
    }
  }
  override def getIdent: Ident = ident
}