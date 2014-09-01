package ModuleSyntax

import Compiler.Ident

case class Signature(ident:Ident, value: List[Declaration])
