package ModuleSyntax

import Compiler.Ident

case class Signature(ident:Ident, values: List[Declaration])
