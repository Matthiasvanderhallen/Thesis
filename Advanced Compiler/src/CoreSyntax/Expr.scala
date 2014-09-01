package CoreSyntax

import Compiler.{TranslationHelper, Translation, Ident}
import ModuleSyntax._
import TypeSystem._

sealed trait Expr {
  def translate(trans:Translation, structure:Structure, signature:Signature, argTypes:List[(Type, String)], retType:String):String = {
    var body = ""
    var types = Map[String, Type]()
    var lets = Map[Ident,String]()
    var temp = 0

    for(arg <- argTypes)
      arg match{
        case (argType, argName) => {

          val argTypeStr = argType.getTypeString(structure)

          if(argType == TypeSystem.Integer){
            body += s"\t${argName}.val = add %int 0, ${argName} ; Integer reassign hack\n"
          }else{
            body += s"\t${argName}.val = load ${argTypeStr} ${argName}\n"
          }

          types = types + (s"${argName}" -> argType)
        }
      }

    def localize(origin: String, typ:Type):String = typ match{
      case StructType(id,_,_) => {
        if(id.value.contains(".")){
          s"%$id*"
        }else{
          s"%${origin}.${id.value}*"
        }
      }
      case TypeSystem.Integer => TypeSystem.Integer.toString()
      case x@_ => s"${x.toString()}*"
    }

    def translateExp(exp:Expr):String = {
      exp match{
        case ConstExpr(value) => {
          body += s"\t%t$temp = add %int 0, ${value} ; Hack to give ints a name \n "

          types = types + (s"%t$temp" -> TypeSystem.Integer)
          temp = temp + 1
          s"%t${temp-1}"
        }
        case CallExpr(name, args) => {
         val funcName = name.internalize(structure) //Find the function name. Either it is local, or it already is a qualified name
         val origin = funcName.split("""\.""")(0) //Get the qualification part from the func name. This is the defining structure for any type in

         val funcType = trans.getFuncType(structure, name); //Find the function type associated with the function. Pass the current structure, because the function name might not be a qualified name.

         val argIds = args.map(argExp => translateExp(argExp)) //Translate the expressions and give back the bindings, example: //List("%t1", "%t2")
         val argData = funcType.left.zip(argIds) // List[(Expected TypeSystem.Type, LLVM Identifier pre conversion)]
         var argCallData = List[(String, String)]() //List[(Expected TypeSystem.Type in String, LLVM Identifier post conversion)]

         //Possibly, these values are (according to LLVM) not yet of the right type. In that case, we need to cast them.
         for((expectedType, identifier) <- argData){
           if(types.get(identifier).get != expectedType){
             if(expectedType == TypeSystem.Integer){ // If the expected type is an integer, the current type MUST be of type {%int}, so extract the int.
              body += "\n"
              body += s"\t%t${temp}.${identifier.substring(1)}.val = load ${types.get(identifier).get.getTypeString(structure)} ${identifier} \n"
              body += s"\t%t${temp}.${identifier.substring(1)} = extractvalue %${types.get(identifier).get.qualifiedName(structure)} %t${temp}.${identifier.substring(1)}.val, 0 \n"
             }else if(expectedType.isInstanceOf[VarType]){ // If the expected type is a type variable, tyvarize it. We already are sure the current type associated with the identifier isn't a typevar.
               body += "\n" + TranslationHelper.tyvarize(s"%t${temp}.${identifier.substring(1)}", identifier, types.get(identifier).get, structure) + "\n"
             }else{ //bitcast.
               body += "\n" + s"\t%t${temp}.${identifier.substring(1)} = bitcast ${types.get(identifier).get.getTypeString(structure)} ${identifier} to ${localize(origin,expectedType)}\n"+ "\n"
             }
             argCallData = argCallData :+ (localize(origin,expectedType), s"%t${temp}.${identifier.substring(1)}")// build call data with adjusted identifier //De funcType annotatie komt van structure met naam origin, niet perse huidige structure
           }else{
             argCallData = argCallData :+ (localize(origin,expectedType), identifier)// build call data with unchanged identifier //De funcType annotatie komt van structure met naam origin, niet perse huidige structure
           }
         }

         val argCallString = argCallData.map{case (a,b) => s"$a $b"}.mkString(", ") //build the argument string

         body += s"\t%t$temp = call ${localize(origin, funcType.right)} @${funcName}_internal(${argCallString})\n" //perform the call

         types = types + (s"%t$temp" -> funcType.right) //TODO: Might need to change ident to the qualified version?
         temp = temp+1
         return s"%t${temp-1}"
        }
        case BinOpExpr(op, left, right) => {
          var leftbind = translateExp(left)
          var rightbind = translateExp(right)

          if(types.get(leftbind).get != TypeSystem.Integer){
            body += s"\t%t${temp}.left.int = extractvalue %${types.get(leftbind).get.qualifiedName(structure)} $leftbind.val, 0 ; Access \n"
            leftbind = s"%t${temp}.left.int"
          }

          if(types.get(rightbind).get != TypeSystem.Integer){
            body += s"\t%t${temp}.right.int = extractvalue %${types.get(rightbind).get.qualifiedName(structure)} $rightbind.val, 0 ; Access\n"
            rightbind = s"%t${temp}.right.int"
          }

          op match{
            case Add => body += s"\t%t$temp = add %int $leftbind, $rightbind\n"
            case Sub => body += s"\t%t$temp = sub %int $leftbind, $rightbind\n"
            case Mul => body += s"\t%t$temp = mul %int $leftbind, $rightbind\n"
            case Rem => body += s"\t%t$temp = srem %int $leftbind, $rightbind\n"
          }

          types = types + (s"%t$temp" -> TypeSystem.Integer)
          temp = temp+1
          return s"%t${temp-1}"
        }
        case LetExpr(ident, bind, in) => {
          val bnd = translateExp(bind);
          lets = lets + (ident->bnd)
          return translateExp(in)
        }
        case ValExpr(ident) =>{

          if(lets.contains(ident)){ //Bound by a local let expression
            return lets.get(ident).get
          }else if(argTypes.map{case (x,y)=>y; case _ => {}}.contains(s"%${ident.value}")){ //One of the arguments
            //val name = s"%${ident.value}"
            return s"%${ident.value}"//return argTypes.find{case (_,`name`) => true; case _ => false}.get._2
          }else{ //Bound by this structure or another structure
            val valName = ident.internalize(structure)
            val origin = valName.split("""\.""")(0)

            val valType = trans.getValType(structure, ident)
            body += s"\t%t$temp = call ${localize(origin, valType)} @${valName}_internal()\n"

            //load value
            if(valType == TypeSystem.Integer){
              body +=s"\t%t$temp.val = add %int 0, %t$temp\n"
            }else{
              body +=s"\t%t$temp.val = load ${localize(origin, valType)} %t$temp\n"
            }

            types = types + (s"%t$temp" -> valType)
            temp = temp + 1 //hoog temp op

            return s"%t${temp-1}"
          }
        }
        case PairExpr(left,right) => {
          val leftbind = translateExp(left)
          val rightbind = translateExp(right)

          body += s"\t%t${temp}.addr = call i8* @malloc(%int 16)\n"
          body += s"\t%t${temp} = bitcast i8* %t${temp}.addr to %pair*\n"

          if(types.get(leftbind).get.isInstanceOf[VarType]){
            body += s"\t%t${temp}.0 = insertvalue %pair undef, %tyvar* ${leftbind},0  ; insert left tyvar in pair\n"
          }else{
            body += TranslationHelper.tyvarize(s"%t${temp}.l", leftbind, types.get(leftbind).get, structure)
            body += s"\t%t${temp}.0 = insertvalue %pair undef, %tyvar* %t${temp}.l, 0 ; insert left tyvar in pair\n" //TODO
          }

          if(types.get(rightbind).get.isInstanceOf[VarType]){
            body += s"\t%t${temp}.1 = insertvalue %pair %t${temp}.0, %tyvar* ${rightbind},1 ; insert right tyvar in pair\n"
          }else{
            body += TranslationHelper.tyvarize(s"%t${temp}.r", rightbind, types.get(rightbind).get, structure)
            body += s"\t%t${temp}.0 = insertvalue %pair %t${temp}.0, %tyvar* %t${temp}.r, 1 ; insert right tyvar in pair\n" //TODO
          }

          body += s"\tstore %pair %t${temp}.1, %pair* %t${temp} ; Store pair to pointer\n"
          body += s"\t%t${temp}.val = load %pair* %t${temp}  ; load pair from pointer for later use\n"

          types = types + (s"%t$temp" -> PairType(types.get(leftbind).get,types.get(rightbind).get))
          temp = temp+1
          return s"%t${temp-1}"
        }
        case PairAccess(pair, annot, side) => {
          val pairbnd = translateExp(pair)

          //bitcast to pair* if type not pair* //TODO: extractable
          if(! types.get(pairbnd).get.isInstanceOf[PairType]){ //If the bound var is of a different type than the pair type -> LLVM code to convert
            if(types.get(pairbnd).get.isInstanceOf[VarType]){ // If it is a tyvar, load the tyvar, get the address as an int, and cast it to a pair.
              body +=s"\t%t${pairbnd}.${temp}.val = load %tyvar* $pairbnd\n"
              body +=s"\t%t${temp}.pair.addr = extractvalue %tyvar $pairbnd.${temp}.val, 0 ; extract address from tyvar\n"
              body +=s"\t%t${temp}.pair = inttoptr %int %t${temp}.pair.addr to %pair* ; cast address to pair*\n" // bitcast -> inttoptr
            }else{ //If it is not a tyvar, it must be a pointer type, which is bitcastable to a pair* pointer.
              body +=s"\t%t${temp}.pair = bitcast ${types.get(pairbnd).get.getTypeString(structure)} $pairbnd to %pair*\n" //Changed + *, the original type can never be a non-pointer type
            }
            body += s"\t%t${temp}.pair.val = load %pair* %t${temp}.pair\n"
          }else{
            body += s"\t%t${temp}.pair.val = load %pair* %t${pairbnd}\n"
          }

          body += s"\t%t${temp}.tyvar.ptr = extractvalue %pair %t${temp}.pair.val, ${side.loc}\n" // This loads the %tyvar pointer value inside the pair
          //body += s"\t%t${temp}.tyvar.ptr = inttoptr %int %t${temp}.tyvar.addr to %tyvar*\n" //Changed bitcast -> inttoptr

          body += TranslationHelper.untyvarize(s"%t${temp}", s"%t${temp}.tyvar.ptr", annot, structure)

          types = types + (s"%t$temp" -> annot)
          temp = temp+1;
          return s"%t${temp-1}"
        }
        case Empty(annot) => {
          body +=s"\t%t${temp}.addr = call i8* @malloc(%int 1600)\n"
          body +=s"\t%t${temp} = bitcast i8* %t${temp}.addr to %array*\n"
          body +=s"\t%t${temp}.val.0 = insertvalue %array undef, %int 0, 0\n"
          body +=s"\t%t${temp}.val.1 = insertvalue %array %t${temp}.val.0, %tyvar* null,1\n"
          body +=s"\tstore %array %t${temp}.val.1, %array* %t${temp}\n"

          types = types + (s"%t${temp}" -> ListType(annot))
          temp = temp+1
          return s"%t${temp-1}"
        }
        case ConsExpr(elem:Expr, tail:Expr) => {

          var elembnd = translateExp(elem)
          val tailbnd = translateExp(tail)
          val arraybnd = s"%t${temp}"


          body += TranslationHelper.getAsArray(s"%t${temp}", tailbnd, types.get(tailbnd).get, structure)
          temp = temp +1

          body += s"\t%t${temp}.array.length = extractvalue %array ${arraybnd}.val, 0\n"
          body += s"\t%t${temp}.array.elem.ptr = getelementptr %array* ${arraybnd}, i32 0, i32 1, %int %t${temp}.array.length\n"

          if(types.get(elembnd).get.isInstanceOf[VarType]){
            body += s"store %tyvar* ${elembnd}, %tyvar** %t${temp}.array.elem.ptr\n"
          }else{
            body += TranslationHelper.tyvarize(s"%t${temp}.elem", elembnd, types.get(elembnd).get, structure)
            body += s"\tstore %tyvar* %t${temp}.elem, %tyvar** %t${temp}.array.elem.ptr\n"
          }

          body += s"\t%t${temp}.array.length.ptr = getelementptr %array* ${arraybnd}, i32 0, i32 0\n"
          body += s"\t%t${temp}.array.length.new = add %int 1, %t${temp}.array.length\n"
          body += s"\tstore %int %t${temp}.array.length.new, %int* %t${temp}.array.length.ptr\n"

          temp = temp+1

          return tailbnd
        }
        case ListIndex(list:Expr, index:Int, annot:Type) => {
          var listbnd = translateExp(list)

          body += TranslationHelper.getAsArray(s"%t${temp}", listbnd, types.get(listbnd).get, structure)
          listbnd = s"%t${temp}"

          temp = temp+1

          body += s"\t%t${temp}.array.length = extractvalue %array %t${listbnd}.val, 0\n"
          body += s"\t%t${temp}.check = icmp slt %int $index, %t${temp}.array.length\n"
          body += s"\tbr i1 %t${temp}.check, label %Continue$temp, label %Exit$temp \n\n"

          body += s"\tError$temp:\n" +
                  "\t\tcall void @exit(i32 -1)\n" +
                  "\t\tunreachable\n\n"

          body += s"Continue${temp}:\n"
          body += s"\t%t${temp}.tyvar.ptr.ptr = getelementptr %array* $listbnd, i32 0, i32 1, %int $index\n"
          body += s"\t%t${temp}.tyvar.ptr = load %tyvar** %t${temp}.tyvar.ptr.ptr\n"

          body += TranslationHelper.untyvarize(s"%t${temp}", s"%t${temp}.tyvar.ptr", annot, structure)

          types = types + (s"%t${temp}" -> annot)
          temp = temp+1
          return s"%t${temp-1}"

        }
        case expr@_ => {throw new UnsupportedOperationException(s"Expression $expr could not be handled")}
      }
    }


    var retval = translateExp(this)

    if(types.get(retval).get.getTypeString(structure) != retType){
      if(types.get(retval).get == TypeSystem.Integer){
        body += s"\t%t$temp.addr = call i8* @malloc(%int 8)\n"
        body += s"\t%t$temp = bitcast i8* %t$temp.addr to ${retType}\n"
        body += s"\t%t$temp.val = insertvalue ${retType.replace("*","")} undef, %int $retval, 0\n"
        body += s"\tstore ${retType.replace("*","")} %t$temp.val, ${retType} %t$temp\n"
        retval = s"%t$temp"
      }else{
        if(retType == TypeSystem.Integer.toString()){
          body += s"\t%t$temp.val = load ${types.get(retval).get.getTypeString(structure)} $retval\n"
          body += s"\t%t$temp = extractvalue %${types.get(retval).get.qualifiedName(structure)} %t$temp.val, 0\n"
          retval = s"%t$temp"
          //body += s"\t%t$temp = bitcast ${types.get(retval).get.getTypeString(structure)} $retval.val to $retType\n"

        } else{
          body += s"\t%t$temp = bitcast ${types.get(retval).get.getTypeString(structure)} $retval to $retType\n"
          retval = s"%t$temp"
        }
      }
    }
    body += s"\tret $retType $retval\n"

    return body
  }
}
case class ValExpr(ident:Ident) extends Expr
case class ConstExpr(value: Int) extends Expr
case class CallExpr(name:Ident, args: List[Expr]) extends Expr
case class BinOpExpr(op:BinOp, left:Expr, right:Expr) extends Expr
case class LetExpr(ident:Ident, bind:Expr, in:Expr) extends Expr
case class PairExpr(left:Expr, right:Expr) extends Expr
//case class LeftExpr(pair:Expr, annot:TypeSystem.Type) extends Expr
//case class RightExpr(pair:Expr, annot:TypeSystem.Type) extends Expr
case class PairAccess(pair:Expr, annot:Type, side:Side) extends Expr
//sealed trait ListExpr extends Expr
case class Empty(annot:Type) extends Expr
case class ConsExpr(elem:Expr, tail:Expr) extends Expr
case class ListIndex(list:Expr, index:Int, annot:Type) extends Expr