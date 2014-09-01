package ModuleSyntax

import TypeSystem.Type
import Compiler.Ident

sealed trait Declaration
sealed trait TypeDeclaration extends Declaration
case class TransparentTypeDeclaration(ident:Ident, tyvars:Int, definition: Type) extends TypeDeclaration
case class OpaqueTypeDeclaration(ident:Ident, tyvars:Int) extends TypeDeclaration
case class ValDeclaration(ident:Ident, ascription: Type) extends Declaration