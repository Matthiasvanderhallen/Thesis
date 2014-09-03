package Compiler

import ModuleSyntax.{StructureDefinition, Structure}

sealed trait MLToken
case class Ident(value: String) extends MLToken with Ordered[Ident]{
  override def toString() = value;
  def ord():Int = {
    return value.charAt(0).toInt-97;
  }

  def internalize(structureDef:StructureDefinition):String = {
    if(value.contains("."))
      return value;
    else
      return s"${structureDef.ident.value}.${value}"
  }

  def compare(a:Ident) = value.compareTo(a.value)
}
case class Number(value: Int) extends MLToken
case object Delimiter extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal