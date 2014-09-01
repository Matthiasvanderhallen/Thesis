package ModuleSyntax

import Compiler.Ident

case class Functor(ident:Ident, argIdent:Ident, argSignature:Ident, signature:Ident, value: List[Definition])
