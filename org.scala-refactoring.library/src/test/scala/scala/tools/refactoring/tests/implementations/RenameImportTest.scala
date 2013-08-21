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
    
    import java.io.{InputStream, /*(*/OutputStream/*)*/, File}
    
    case class IO(in: InputStream, out: OutputStream)
    """ becomes
    """
    package renameImport.secondFromMultipleSelectors
    
    import java.io.{InputStream, /*(*/OutputStream/*)*/ => OS, File}
    
    case class IO(in: InputStream, out: OS)
    """ 
  } applyRefactoring(renameImportTo("OS"))
  
  @Test(expected = classOf[PreparationException])
  def failOnAlreadyRenamed = new FileSet {
    """
    package renameImport.failOnAlreadyRenamed
    
    import java.io.{/*(*/InputStream/*)*/ => IS}
    
    class C(val in: IS)
    """ becomes
    """
    package renameImport.failOnAlreadyRenamed
    
    import java.io.{/*(*/InputStream/*)*/ => IS}
    
    class C(val in: IS)
    """
  } applyRefactoring(renameImportTo("JInputStream"))
  
  @Test(expected = classOf[PreparationException])
  def failOnNonImport = new FileSet {
    """
    package renameImport.failOnNonImport
    
    import java.io.{InputStream}
    
    class C(val in: InputStream)
    """ becomes
    """
    package renameImport.failOnNonImport
    
    import java.io.{InputStream}
    
    class C(val in: InputStream)
    """
  } applyRefactoring(renameImportTo("IS"))
  
}