sealed trait Type{
  def intRepresentation:Int
  def getVarTypes(trans:Translation):List[Int]
  def qualifiedName(structure:Structure):String //Structname.typename, array, pair, int
  def getTypeString(structure:Structure):String //%Structname.typename*, %array*, %pair*, %int
}

case object Integer extends Type {
  override def toString() = "%int"
  def intRepresentation = 1;
  def getVarTypes(trans:Translation):List[Int] = {
    return List()
  }
  def qualifiedName(structure:Structure):String = {
    return "int"
  }

  def getTypeString(structure:Structure):String = {
    return toString() //No pointers to ints!
  }
}

case class FuncType(left:List[Type], right:Type) extends Type{
  def intRepresentation = 5;
  def getVarTypes(trans:Translation):List[Int] = {
    throw new UnsupportedOperationException();
  }
  def qualifiedName(structure:Structure):String = {
    throw new UnsupportedOperationException("Can't internalize a functype");
  }
  def getTypeString(structure:Structure):String = {
    throw new UnsupportedOperationException("Functions are not a first class value, and are not passable")
  }
}

case class VarType(ident:Ident) extends Type {
  override def toString() = "%tyvar"
  def intRepresentation = 2;

  def getVarTypes(trans:Translation):List[Int] = {
    return List(ident.value.charAt(0).toInt-97)
  }

  def qualifiedName(structure:Structure):String = {
    throw new UnsupportedOperationException("Can't internalize a VarType");
  }

  def getTypeString(structure:Structure):String = {
    return "%tyvar*"
  }
}

case class StructType(typeId:Ident, nrTyvars:Int, tyvars:List[Type]) extends Type {
  def intRepresentation = -1; //Hmm

  def qualifiedName(structure:Structure):String = {
    if(typeId.value.contains("."))
      return s"${typeId.value}"
    else
      return s"${structure.ident.value}.${typeId.value}"
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
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
  def qualifiedName(structure:Structure):String = {
    return "pair"; //Mogelijkerwijs + *
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
  }
}

case class ListType(inner:Type) extends Type {
  override def toString() = "%array"
  def intRepresentation = 4;

  def getVarTypes(trans:Translation):List[Int] = {
    return inner.getVarTypes(trans)
  }

  def qualifiedName(structure:Structure):String = {
    return "array"; //Mogelijkerwijs + *
  }

  def getTypeString(structure:Structure):String = {
    return s"%${qualifiedName(structure)}*"
  }
}