package scala.tools.refactoring.implementations

abstract class RenameOverloaded extends Rename {

  import global._
  
  override def prepare(s: Selection) = {
    s.selectedTopLevelTrees
    val selectedSymbolTree = s.selectedSymbolTree
    val selectedSymbol = selectedSymbolTree.map(findSelectedSymbol(_, s))
    val processor = (selectedSymbolTree, selectedSymbol) match {
      case (Some(t), Some(sym)) => Some(new OverloadedProcessor(t, sym))
      case (_, _) => None
    }
    
    val result = processor.map(PreparationResult(_, false))
    
    result.toRight(PreparationError("no selected tree/symbol found"))
  }
  
  class OverloadedProcessor(selectedTree: SymTree, selectedSymbol: Symbol) 
    extends DefaultProcessor(selectedTree, selectedSymbol) {
    
    override def treesToRename = {
      val rawOwner = index.declaration(selectedSymbol).map(_.symbol.ownerChain).getOrElse(Nil).filter(_.isType).headOption
      val owner = rawOwner.map(o => if(o.isModuleClass) o.module else o)
      val parents = owner.toList.flatMap(o => o::o.parentSymbols)
      val ownerTrees = parents.flatMap(index.declaration(_))
      val definitions = ownerTrees flatMap {
        ot => ot match {
          case cd: ClassDef => 
            cd.impl.body collect {
              case d: DefDef if d.symbol.nameString == selectedSymbol.nameString => d.symbol
            }
          case md: ModuleDef =>
            md.impl.body collect {
              case d: DefDef if d.symbol.nameString == selectedSymbol.nameString => d.symbol
            }
          case _ => 
            Nil
        }
      }
      val allOccurrences = definitions.flatMap(index.occurences(_))
      allOccurrences.toSet.toList // get rid of duplicates
    }
    
  }
  
}