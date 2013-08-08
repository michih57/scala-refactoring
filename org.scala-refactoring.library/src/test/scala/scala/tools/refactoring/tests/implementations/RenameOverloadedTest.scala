package scala.tools.refactoring
package tests.implementations

import scala.tools.refactoring.implementations.RenameOverloaded
import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.tests.util.TestRefactoring

class RenameOverloadedTest extends TestHelper with TestRefactoring {

  def renameOverloadedTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new RenameOverloaded with SilentTracing with TestProjectIndex
    val changes = performRefactoring(name)
  }.changes
  
  @Test
  def simpleOverloaded = new FileSet {
    """
    package renameOverloaded.simpleOverloaded
    
    class O {
      def /*(*/foo/*)*/() = 57
      def foo(i: Int) = i
    }
    """ becomes
    """
    package renameOverloaded.simpleOverloaded
    
    class O {
      def /*(*/bar/*)*/() = 57
      def bar(i: Int) = i
    }
    """
  } applyRefactoring(renameOverloadedTo("bar"))
  
}