package ProgramSyntax
import Compiler.Translation
import ModuleSyntax.{Signature, Structure}

case class Program(val signatures:List[Signature], val structures:List[Structure]){
  def translate(trans:Translation):Translation = {
    var translation = trans

    val structures = this.structures.sortBy(struct => struct.ident.value)

    for (structure <- structures) {
      val sig = signatures.find(sig => sig.ident == structure.signature).get
      translation = structure.translate(translation, sig);
    }

    return translation;
  }
}