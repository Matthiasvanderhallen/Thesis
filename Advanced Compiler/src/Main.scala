import Compiler.Translation
import CoreSyntax._
import ModuleSyntax._
import ProgramSyntax.Program
import TypeSystem.{ListType, PairType, Type, VarType}
import com.sun.tools.javac.code.Type.TypeVar

object Main extends App{
  val program = Programs.program1

  println(translate(program))

  var entrypoints:String = ""

  def translate(program:Program):Translation={
    var translation = new Translation(program, true);

    //First translation pass.
    translation = program.translate(translation);
    translation.firstPass = false;

    //Second translation pass.
    translation = program.translate(translation);

    return translation;
  }
}
//Zou ook rekening kunnen houden bij call om dan te checken of het unexpected type miss een tyvar is, en dan geen annotaties nodig bij het uitpakken van een pair/array, omdat je tyvar kan gebruiken. Then again, wat met tyvars van een verschillend type? Welke 'ident'?
//Implementation type must be Liskov substitution principle subtype of declaration type.
// For functions:
//    1. implementation has tyvar argument while declaration specifies specific type. (Contravariance)
//    2. implementation has specific returntype while declaration is more general. (Covariance)
// This has already mostly been implemented because we tyvarize an argument when it isn't a tyvar when it's supposed to. (Contravariance)
// TODO: The second doesn't really happen in normal situations? (i.e. you never have return type typevar a, unless a was featured in the type earlier. But then it's an argument, and this can't be replaced by the more specific type, as it breaks contravariance)
