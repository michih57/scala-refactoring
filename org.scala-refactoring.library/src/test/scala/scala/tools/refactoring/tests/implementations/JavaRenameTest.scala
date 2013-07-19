/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.Rename
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import org.junit.Ignore

import language.reflectiveCalls

class JavaRenameTest extends TestHelper with TestRefactoring {
  outer =>
  
  def renameTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new Rename with SilentTracing with TestProjectIndex
    val changes = performRefactoring(name)
  }.changes
  
  @Test
  def renameClass = new FileSet {
    """
      package rename.javaClass
    
      case class SUser{
        val j: java.io.File = new java.io./*(*/File/*)*/("asdf")
      }
      
    """ becomes
    """
      package rename.javaClass
    
      case class SUser{
        val j: java.io.JFile = new java.io./*(*/JFile/*)*/("asdf")
      }
      
    """
  } applyRefactoring(renameTo("JFile"))
  
  @Test
  def renameEnum = new FileSet {
    """
    package rename.javaEnum
    
    class SUser {
      val r: java.math./*(*/RoundingMode/*)*/ = java.math.RoundingMode.CEILING
    }
    """ becomes
    """
    package rename.javaEnum
    
    class SUser {
      val r: java.math./*(*/RoundStrategy/*)*/ = java.math.RoundStrategy.CEILING
    }
    """
  } applyRefactoring(renameTo("RoundStrategy"))
  
  @Test
  def renameImported = new FileSet {
    """
    package rename.imported
    
    import java.io.File
    
    class SUser {
      val f: /*(*/File/*)*/ = new File("foo.txt")
    }
    """ becomes
    """
    package rename.imported
    
    import java.io.JFile
    
    class SUser {
      val f: /*(*/JFile/*)*/ = new JFile("foo.txt")
    }
    """
  } applyRefactoring(renameTo("JFile"))
  
  @Test
  def renameImportedAtImport = new FileSet {
    """
    package rename.imported
    
    import java.io./*(*/File/*)*/
    
    class SUser {
      val f: File = new File("foo.txt")
    }
    """ becomes
    """
    package rename.imported
    
    import java.io./*(*/JFile/*)*/
    
    class SUser {
      val f: JFile = new JFile("foo.txt")
    }
    """
  } applyRefactoring(renameTo("JFile"))
  
  @Test
  def renameImportedFromMultipleImports = new FileSet {
    """
    package rename.importedFromMultipleImports
    
    import java.io.{InputStream, /*(*/File/*)*/}
    
    class SUser {
      val f: File = new File("foo.txt")
    }
    """ becomes
    """
    package rename.importedFromMultipleImports
    
    import java.io.{InputStream, /*(*/JFile/*)*/}
    
    class SUser {
      val f: JFile = new JFile("foo.txt")
    }
    """
  } applyRefactoring(renameTo("JFile"))
  
}