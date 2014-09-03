package TypeSystem

import ModuleSyntax.{StructureDefinition, Structure}
import Compiler.{Ident, Translation}

sealed trait Type {
   def toString():String
   def intRepresentation:Int
   def getVarTypes(trans:Translation):List[Int]
   def qualifiedName(structureDef:StructureDefinition):String //Structname.typename, array, pair, int
   def getTypeString(structureDef:StructureDefinition):String //%Structname.typename*, %array*, %pair*, %int
 }

case object Integer extends Type {
  override def toString() = "%int"
  def intRepresentation = 1;
  def getVarTypes(trans:Translation):List[Int] = {
    return List()
  }
  def qualifiedName(structureDef:StructureDefinition):String = {
    return "int"
  }

  def getTypeString(structureDef:StructureDefinition):String = {
    return toString() //No pointers to ints!
  }
}

case class FuncType(left:List[Type], right:Type) extends Type{
  def intRepresentation = 5;
  def getVarTypes(trans:Translation):List[Int] = {
    throw new UnsupportedOperationException();
  }
  def qualifiedName(structureDef:StructureDefinition):String = {
    throw new UnsupportedOperationException("Can't internalize a functype");
  }
  def getTypeString(structureDef:StructureDefinition):String = {
    throw new UnsupportedOperationException("Functions are not a first class value, and are not passable")
  }
}

case class VarType(ident:Ident) extends Type {
  override def toString() = "%tyvar"
  def intRepresentation = 2;

  def getVarTypes(trans:Translation):List[Int] = {
    return List(ident.value.charAt(0).toInt-97)
  }

  def qualifiedName(structureDef:StructureDefinition):String = {
    throw new UnsupportedOperationException("Can't internalize a VarType");
  }

  def getTypeString(structureDef:StructureDefinition):String = {
    return "%tyvar*"
  }
}

case class StructType(typeId:Ident, nrTyvars:Int, tyvars:List[Type]) extends Type {
  def intRepresentation = -1; //Hmm

  def qualifiedName(structureDef:StructureDefinition):String = {
    if(typeId.value.contains("."))
      return s"${typeId.value}"
    else
      return s"${structureDef.ident.value}.${typeId.value}"
  }

  def getTypeString(structureDef:StructureDefinition):String = {
    return s"%${qualifiedName(structureDef)}*"
  }

  def getVarTypes(trans:Translation):List[Int] = {
    return trans.getImplType(trans.getOpaqueType(typeId.value)).getVarTypes(trans)
  }

  override def toString() = typeId.value
}

case class PairType(left:Type, right:Type) extends Type {
  override def toString() = "%pair"

  def intRepresentation = 3;

  def getVarTypes(trans:Translation):List[Int] = {
    return left.getVarTypes(trans)++right.getVarTypes(trans)
  }
  def qualifiedName(structureDef:StructureDefinition):String = {
    return "pair"; //Mogelijkerwijs + *
  }

  def getTypeString(structureDef:StructureDefinition):String = {
    return s"%${qualifiedName(structureDef)}*"
  }
}

case class ListType(inner:Type) extends Type {
  override def toString() = "%array"
  def intRepresentation = 4;

  def getVarTypes(trans:Translation):List[Int] = {
    return inner.getVarTypes(trans)
  }

  def qualifiedName(structureDef:StructureDefinition):String = {
    return "array"; //Mogelijkerwijs + *
  }

  def getTypeString(structureDef:StructureDefinition):String = {
    return s"%${qualifiedName(structureDef)}*"
  }
}