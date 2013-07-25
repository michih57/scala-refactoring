/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.TreeAnalysis
import tools.nsc.symtab.Flags
import scala.tools.refactoring.common.PimpedTrees
import scala.collection.immutable.ListMap

abstract class Rename extends MultiStageRefactoring with TreeAnalysis with analysis.Indexes with TreeFactory with common.InteractiveScalaCompiler with PimpedTrees {
    
  import global._

  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)

  type RefactoringParameters = String

  def prepare(s: Selection) = {
    s.selectedSymbolTree match {

      // Has been renamed.. also check for a matching importselector that did the rename
      case Some(t: RefTree) if t.name != t.symbol.name =>
        Right(PreparationResult(t, true))

      case Some(t) =>
        val isLocalRename = (t.symbol.isPrivate || t.symbol.isLocal) && !t.symbol.hasFlag(Flags.ACCESSOR)
        Right(PreparationResult(t, isLocalRename))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }

  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
    trace("Selected tree is %s", prepared.selectedTree)

    val sym = findSelectedSymbol(prepared.selectedTree, selection)
    
    val occurences = index.occurences(sym) 
    
    occurences foreach (s => trace("Symbol is referenced at %s (%s:%s, %s:%s)", 
        s, s.pos.source.file.name, s.pos.line, s.pos.start, s.pos.end))

    val isInTheIndex = filter {
      case t: Tree => occurences contains t
    }

    val renameTree = transform {
      case t: ImportSelectorTree =>
        mkRenamedImportTree(t, newName)
      case t: SymTree => 
        val annotations = t.symbol.annotations
        val referenced = annotations.find(a => a.symbol == sym)
        val renamed = referenced match {
          case Some(ann) =>
            val duplicate = t.duplicate
            duplicate.symbol.removeAnnotation(sym)
            val tpe = new Type {
              override def safeToString = newName
            }
            val renamedAnn = Annotation(tpe, List()/*ann.scalaArgs*/, ann.javaArgs) setPos NoPosition
            duplicate.symbol.addAnnotation(renamedAnn)
            duplicate
          case None => mkRenamedSymTree(t, newName) setPos (t.pos withStart t.pos.start)
        }
        
        renamed replaces t
        
      case t: TypeTree => 
        mkRenamedTypeTree(t, newName, prepared.selectedTree.symbol)
      case t @ Literal(Constant(value: TypeRef)) if isClassTag(t.value) =>
        val OriginalSymbol = prepared.selectedTree.symbol
        val newType = value map {
          case TypeRef(pre, OriginalSymbol, args) =>
            // Uh..
            new Type {
              override def safeToString: String = newName
            }
          case t => t
        }
        Literal(Constant(newType)) replaces t
    }

    val rename = topdown(isInTheIndex &> renameTree |> id)

    val renamedTrees = occurences flatMap (rename(_))

    Right(refactor(renamedTrees))
  }
  
  def findSelectedSymbol(selectedTree: SymTree, selection: Selection): Symbol = {
    implicit def intToRangeInclusionTester(i: Int) = new AnyRef {
      def includedIn(position: Position) = position.start <= i && position.end >= i
    }

    val treeSymbol = selectedTree.symbol
    
    selectedTree match {
      case Import(expr, selectors) => {
        val markedSelector = selectors.find{case ImportSelector(_, namePos, _, _) => namePos.includedIn(selection.pos)}
        markedSelector.flatMap(s => findSymbolForImportSelector(expr, s.name)).getOrElse(treeSymbol)
      }
      case _ if selection.isAnnotationSelected => selection.selectedAnnotation.getOrElse(treeSymbol)
      case _ => treeSymbol
    }
  }
  
  
}
