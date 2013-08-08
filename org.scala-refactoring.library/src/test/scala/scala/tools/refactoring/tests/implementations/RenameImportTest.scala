package scala.tools.refactoring
package tests.implementations

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.implementations.RenameImport

class RenameImportTest extends TestHelper with TestRefactoring {

  def renameImportTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new RenameImport with SilentTracing with TestProjectIndex
    val changes = performRefactoring(name)
  }.changes
  
  @Test
  def singleSelector = new FileSet {
    """
    package renameImport.singleSelector
    
    import java.io./*(*/File/*)*/
    
    case class FileWrapper(f: File)
    """ becomes
    """
    package renameImport.singleSelector
    
    import java.io.{/*(*/File => JFile/*)*/}
    
    case class FileWrapper(f: JFile)
    """
  } applyRefactoring(renameImportTo("JFile"))
  
  @Test
  def firstFromMultipleSelectors = new FileSet {
    """
    package renameImport.firstFromMultipleSelectors
    
    import java.io.{/*(*/InputStream/*)*/, OutputStream}
    
    case class IO(in: InputStream, out: OutputStream)
    """ becomes
    """
    package renameImport.firstFromMultipleSelectors
    
    import java.io.{/*(*/InputStream => IS/*)*/, OutputStream}
    
    case class IO(in: IS, out: OutputStream)
    """ 
  } applyRefactoring(renameImportTo("IS"))
  
  @Test
  def secondFromMultipleSelectors = new FileSet {
    """
    package renameImport.secondFromMultipleSelectors
    
    import java.io.{InputStream, /*(*/OutputStream/*)*/}
    
    case class IO(in: InputStream, out: OutputStream)
    """ becomes
    """
    package renameImport.secondFromMultipleSelectors
    
    import java.io.{InputStream, /*(*/OutputStream/*)*/ => OS}
    
    case class IO(in: InputStream, out: OS)
    """ 
  } applyRefactoring(renameImportTo("OS"))
  
  @Test
  def atUsage = new FileSet {
    """
    package renameImport.atUsage
    
    import java.io.{InputStream, OutputStream}
    
    case class IO(in: InputStream, out: /*(*/OutputStream/*)*/)
    """ becomes
    """
    package renameImport.atUsage
    
    import java.io.{InputStream, OutputStream => OS}
    
    case class IO(in: InputStream, out: /*(*/OS/*)*/)
    """
  } applyRefactoring(renameImportTo("OS"))
  
  @Test
  def dummy = {
    val tree = treeFrom(
    """
    import java.io.{InputStream, OutputStream}
    
    case class IO(in: InputStream, out: /*(*/OutputStream/*)*/)
    """)
    
    println(tree)
  }
  
}