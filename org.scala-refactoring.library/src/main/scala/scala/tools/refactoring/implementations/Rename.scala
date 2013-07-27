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
      
//  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean, renameOnlyImport: Boolean = false, isRenamedAtImport: Boolean = false)
  case class PreparationResult(renameProcessor: RenameProcessor, hasLocalScope: Boolean)
  
  type RefactoringParameters = String
  
  private implicit def intToRangeInclusionTester(i: Int) = new AnyRef {
    def includedIn(position: Position) = position.start <= i && position.end >= i
  }
  
  def prepare(s: Selection) = {
    val selectedTree = s.selectedSymbolTree
    val selectedTreeAndSymbol = selectedTree.map(t => (t, findSelectedSymbol(t, s)))
    selectedTreeAndSymbol match {
      case Some((t, symbol)) if s.isAnnotationSelected =>
        val processor = new AnnotationProcessor(t, symbol)
        Right(PreparationResult(processor, false))
      // Has been renamed.. also check for a matching importselector that did the rename
      case Some((t: RefTree, symbol)) if t.name != t.symbol.name =>
        val processor = new RenamedImportProcessor(t, symbol)
        Right(PreparationResult(processor, true))
      case Some((t, symbol)) =>
        val isLocalRename = (t.symbol.isPrivate || t.symbol.isLocal) && !t.symbol.hasFlag(Flags.ACCESSOR)
        val defaultProcessor = new DefaultProcessor(t, symbol)
        val processor = t match {
          case Import(expr, selectors) =>
            val renameMarked = selectors find {
              case ImportSelector(name, _, rename, renamePos) => renamePos.includedIn(s.pos) && name != rename
            }
            renameMarked.map(marked => new RenamedImportProcessor(t, symbol)).getOrElse(defaultProcessor)
          case _ => defaultProcessor
        }
        Right(PreparationResult(processor, isLocalRename))        
      case None => Left(PreparationError("no symbol selected found"))
    }
  }

  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val processor = prepared.renameProcessor
    trace("Selected tree is %s", processor.selectedTree)
    val renamedTrees = processor.perform(newName)
    Right(refactor(renamedTrees))
  }
  
  def findSelectedSymbol(selectedTree: SymTree, selection: Selection): Symbol = {
    val treeSymbol = selectedTree.symbol
    
    selectedTree match {
      case i@ Import(expr, selectors) => {
        val markedNameSelector = selectors.find{case ImportSelector(_, namePos, _, _) => namePos.includedIn(selection.pos)}
        val markedSelector = markedNameSelector.orElse(selectors.find{case ImportSelector(_, _, _, renamePos) => renamePos.includedIn(selection.pos)})
        markedSelector.flatMap(s => findSymbolForImportSelector(expr, s.name)).getOrElse(treeSymbol)
      }
      case _ if selection.isAnnotationSelected => selection.selectedAnnotation.getOrElse(treeSymbol)
      case _ => treeSymbol
    }
  }

  trait RenameProcessor {
    val selectedTree: SymTree
    val selectedSymbol: Symbol
    val renameImportSelector = mkRenamedImportTree _
    val renameSymTree = mkRenamedSymTree _
    val renameTypeTree = mkRenamedTypeTree _
    val renameClassLiteral = mkRenamedClassLiteral _

    def perform(newName: RefactoringParameters): List[Tree] = {
      val sym = selectedSymbol
      
      val occurences = index.occurences(sym)

      occurences foreach (s => trace("Symbol is referenced at %s (%s:%s, %s:%s)",
        s, s.pos.source.file.name, s.pos.line, s.pos.start, s.pos.end))

      val isInTheIndex = filter {
        case t: Tree => occurences contains t
      }

      val renameTree = transform ({
        case t: ImportSelectorTree => 
          renameImportSelector(t, newName)
        case t: SymTree =>
          renameSymTree(t, newName)
        case t: TypeTree =>
          renameTypeTree(t, newName, sym)
        case t @ Literal(Constant(value: TypeRef)) if isClassTag(t.value) =>  
          renameClassLiteral(newName, sym, t, value)
      })

      val rename = topdown(isInTheIndex &> renameTree |> id)
      val renamedTrees = occurences flatMap (rename(_))
      renamedTrees
    }
  }
  
  class DefaultProcessor(override val selectedTree: SymTree, override val selectedSymbol: Symbol) extends RenameProcessor {
    override val renameSymTree = (t: SymTree, newName: String) => t match {
      // TODO: is there a cleaner way to check if r is referring to
      // a symbol that has been renamed at an import?
      case r: RefTree if r.name.toString.trim != selectedSymbol.name.toString.trim => 
        t
      case _ =>
        mkRenamedSymTree(t, newName) setPos (t.pos withStart t.pos.start)
    }
  }
  
  class RenamedImportProcessor(override val selectedTree: SymTree, override val selectedSymbol: Symbol) extends RenameProcessor {
    override val renameImportSelector = mkReRenamedImportTree _
  }
  
  class AnnotationProcessor(override val selectedTree: SymTree, override val selectedSymbol: Symbol) extends RenameProcessor {
    override val renameSymTree = (t: SymTree, newName: String) => {
      val sym = selectedSymbol
      val annotations = t.symbol.annotations
      val referenced = annotations.find(a => a.symbol == sym)
      
      // TODO make actually work (using t.duplicate doesn't seem to be the solution)
      val renamed = referenced match {
        case Some(ann) =>
          val duplicate = t.duplicate
          duplicate.symbol.removeAnnotation(sym)
          val tpe = new Type {
            override def safeToString = newName
          }
          val renamedAnn = Annotation(tpe, ann.scalaArgs, ann.javaArgs) setPos NoPosition
          duplicate.symbol.addAnnotation(renamedAnn)
          duplicate
        case None => mkRenamedSymTree(t, newName) setPos (t.pos withStart t.pos.start)
      }

      renamed replaces t
    }
  }
    
}
