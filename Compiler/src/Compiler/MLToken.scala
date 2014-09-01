package Compiler

import ModuleSyntax.Structure

sealed trait MLToken
case class Ident(value: String) extends MLToken {
  override def toString() = value;
  def ord():Int = {
    return value.charAt(0).toInt-97;
  }

  def internalize(structure:Structure):String = {
    if(value.contains("."))
      return value;
    else
      return s"${structure.ident.value}.${value}"
  }
}
case class Number(value: Int) extends MLToken
case object Delimiter extends MLToken
case class Keyword(value: String) extends MLToken
case class Literal(value: String) extends MLToken //String Literal