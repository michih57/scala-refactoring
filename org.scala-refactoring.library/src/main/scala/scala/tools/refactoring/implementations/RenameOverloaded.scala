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
      val owner = index.declaration(selectedSymbol).map(_.symbol.ownerChain).getOrElse(Nil).filter(_.isType).headOption
      val ownerTree = owner.flatMap(index.declaration(_)).map(_.asInstanceOf[ClassDef])
      val definitions = ownerTree.toList.flatMap { o =>
        o.impl.body.collect{case d: DefDef if d.symbol.nameString == selectedSymbol.nameString => d.symbol}
      }
      
      
      println(s"""owner: $owner""")
      println(s"""definitions: $definitions""")
      definitions.flatMap(index.occurences(_))   
    }
    
  }
  
}