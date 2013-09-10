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
  def inClass = new FileSet {
    """
    package renameOverloaded.inClass
    
    class O {
      def /*(*/foo/*)*/() = 57
      def foo(i: Int) = i
    }
    """ becomes
    """
    package renameOverloaded.inClass
    
    class O {
      def /*(*/bar/*)*/() = 57
      def bar(i: Int) = i
    }
    """
  } applyRefactoring(renameOverloadedTo("bar"))
  
  @Test
  def inObject = new FileSet {
    """
    package renameOverloaded.inObject
    
    object O {
      def /*(*/m/*)*/(i: Int) = 13
      def m(s: String) = 45
    }
    """ becomes
    """
    package renameOverloaded.inObject
    
    object O {
      def /*(*/method/*)*/(i: Int) = 13
      def method(s: String) = 45
    }
    """
  } applyRefactoring(renameOverloadedTo("method"))
  
  @Test
  def inTrait = new FileSet {
    """
    package renameOverloaded.inTrait
    
    trait T {
      def /*(*/m/*)*/(i: Int) = 13
      def m(s: String) = 45
    }
    """ becomes
    """
    package renameOverloaded.inTrait
    
    trait T {
      def /*(*/method/*)*/(i: Int) = 13
      def method(s: String) = 45
    }
    """
  } applyRefactoring(renameOverloadedTo("method"))
  
  @Test
  def inheritanceParentTrigger = new FileSet {
    """
    package renameOverloaded.inheritanceParentTrigger
    
    class Parent {
      def /*(*/m/*)*/(i: Int) = 13
      def m(s: String) = 45
    }
    
    class Child extends Parent {
      override def m(i: Int) = {
        val double = 2*i
        double*double
      }
    }
    """ becomes
    """
    package renameOverloaded.inheritanceParentTrigger
    
    class Parent {
      def /*(*/method/*)*/(i: Int) = 13
      def method(s: String) = 45
    }
    
    class Child extends Parent {
      override def method(i: Int) = {
        val double = 2*i
        double*double
      }
    }
    """
  } applyRefactoring(renameOverloadedTo("method"))
  
  @Test
  def inheritanceChildTrigger = new FileSet {
    """
    package renameOverloaded.inheritanceChildTrigger
    
    class Parent {
      def m(i: Int) = 13
      def m(s: String) = 45
    }
    
    class Child extends Parent {
      override def /*(*/m/*)*/(i: Int) = {
        val double = 2*i
        double*double
      }
    }
    """ becomes
    """
    package renameOverloaded.inheritanceChildTrigger
    
    class Parent {
      def method(i: Int) = 13
      def method(s: String) = 45
    }
    
    class Child extends Parent {
      override def /*(*/method/*)*/(i: Int) = {
        val double = 2*i
        double*double
      }
    }
    """
  } applyRefactoring(renameOverloadedTo("method"))
  
  @Test
  def withVal = new FileSet {
    """
    package renameOverloaded.withVal
    
    class C {
      def /*(*/m/*)*/(i: Int) = 57
      val m = "booyah"
    }
    """ becomes
    """
    package renameOverloaded.withVal
    
    class C {
      def /*(*/method/*)*/(i: Int) = 57
      val method = "booyah"
    }
    """
  } applyRefactoring(renameOverloadedTo("method"))
  
  @Test
  def inheritanceWithVal = new FileSet {
    """
    package renameOverloaded.inheritanceWithVal
    
    class Parent {
      def bar(i: Int) = {
        2*i
      }
    
      val bar: String => Int = (s: String) => s.length
    }
    
    class Child extends Parent {
      override val /*(*/bar/*)*/: String => Int = (s: String) => 45
    }
    """ becomes
    """
    package renameOverloaded.inheritanceWithVal
    
    class Parent {
      def foo(i: Int) = {
        2*i
      }
    
      val foo: String => Int = (s: String) => s.length
    }
    
    class Child extends Parent {
      override val /*(*/foo/*)*/: String => Int = (s: String) => 45
    }
    """
  } applyRefactoring(renameOverloadedTo("foo")) // TODO: get to bottom of assertion failure
  
}