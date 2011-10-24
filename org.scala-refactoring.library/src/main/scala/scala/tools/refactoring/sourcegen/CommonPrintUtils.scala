/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait CommonPrintUtils {

  this: common.CompilerAccess with AbstractPrinter =>

  import global._
  
  def NL(implicit ctx: PrintingContext): String = {
    if(ctx.file.exists(_.content.containsSlice("\r\n")))
      "\r\n"
    else
      "\n"
  }

  def typeToString(tree: TypeTree, t: Type)(implicit ctx: PrintingContext): String = {
    t match {
      case tpe if tpe == EmptyTree.tpe => ""
      case tpe: ConstantType => tpe.underlying.toString
      case tpe: TypeRef if tree.original != null && tpe.sym.nameString.matches("Tuple\\d+") => 
        tpe.toString
      case tpe if tree.original != null && !tpe.isInstanceOf[TypeRef]=> 
        print(tree.original, ctx).asText
      case r @ RefinedType(parents, _) =>
        parents map {
          case NamedType(name, _)      => name.toString
          case t @ TypeRef(pre, sym, args) => t.toString
          case RefinedType(parents, _) => parents mkString " with "
          case t => throw new Exception("Unhandled type "+ t.getClass.getSimpleName)
        } mkString " with "
      case typeRef @ TypeRef(tpe, sym, arg1 :: ret :: Nil) if definitions.isFunctionType(typeRef) =>
        typeToString(tree, arg1) +" => "+ typeToString(tree, ret)
      case tpe: TypeRef =>
        tpe.toString
      case MethodType(params, result) =>
        val printedParams = params.map(s => typeToString(tree, s.tpe)).mkString(", ")
        val printedResult = typeToString(tree, result)
        
        if(params.size < 1) {
                               "() => "+ printedResult
        } else if(params.size > 1) {
          "(" + printedParams + ") => "+ printedResult              
        } else {
                printedParams +  " => "+ printedResult
        }
        
      case tpe => 
        tpe.toString
    } 
  }
  
        
      def balanceParens(f: Fragment) = Fragment {
        val txt = f.toLayout.withoutComments // TODO also without strings, etc.
        val opening = txt.count(_ == '(')
        val closing = txt.count(_ == ')')
        if(opening > closing && closing > 0) {
          f.asText.reverse.replaceFirst("\\)", ")" * (opening - closing + 1)).reverse
        } else if(opening > closing) {
          f.asText + (")" * (opening - closing))
        } else if(opening < closing) {
          ("(" * (closing - opening)) + f.asText
        } else {
          f.asText
        }
      }

}