package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.common.Change

abstract class RenameImport extends Rename {

  import global._
  
  override def prepare(s: Selection) = {
    val selectedTree = s.selectedSymbolTree
    val prepared = selectedTree flatMap { st => st match {
        case Import(expr, selectors) => 
          val selector = selectors.find(sel => sel.namePos.includedIn(s.pos))
          val selectorSymbol = selector flatMap {
            s => findSymbolForImportSelector(expr, s.name)
          }
          selectorSymbol.map(s => new RenamedImportProcessor(st, s))
        case _ => None
      }
    }
    val preparationResult = prepared.map(PreparationResult(_, true))
    preparationResult.toRight(PreparationError("No selected import selector found."))
  }
  
}