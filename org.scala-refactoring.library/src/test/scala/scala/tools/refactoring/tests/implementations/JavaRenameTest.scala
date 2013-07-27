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
  def renameEnumValue = new FileSet {
    """
    package rename.javaEnumValue
    
    class SUser {
      val r: java.math.RoundingMode = java.math.RoundingMode./*(*/CEILING/*)*/
    }
    """ becomes
    """
    package rename.javaEnumValue
    
    class SUser {
      val r: java.math.RoundingMode = java.math.RoundingMode./*(*/NEW_VALUE/*)*/
    }
    """
  } applyRefactoring(renameTo("NEW_VALUE"))
  
  
  
  @Test
//  @Ignore
  def renameJavaClassAnnotation = new FileSet {
    """
    package renameAnnotation
    
    /*(*/@Deprecated/*)*/
    class Annotated
    """ becomes
    """
    package renameAnnotation
    
    /*(*/@Deprecated/*)*/
    @Override
    class Annotated
    """
  } applyRefactoring(renameTo("Override"))
  
  @Test
//  @Ignore
  def renameImportedJavaClassAnnotation = new FileSet {
    """
    package renameImportedJavaAnnotation
    
    import javax.xml.bind.annotation.XmlRootElement
    
    /*(*/@XmlRootElement/*)*/
    class Element(x: String)
    """ becomes
    """
    package renameImportedJavaAnnotation
    
    import javax.xml.bind.annotation.Root
    
    /*(*/@XmlRootElement/*)*/
    @Root
    class Element(x: String)
    """
  } applyRefactoring(renameTo("Root"))
  
  @Test
//  @Ignore
  def renameJavaValAnnotation = new FileSet {
    """
    package renameValAnnotation
    
    class Annotated {
      /*(*/@Deprecated/*)*/
      val x = 57
    }
    """ becomes
    """
    package renameValAnnotation
    
    class Annotated {
    /*(*/@Deprecated/*)*/
    @Override
    val x = 57
    }
    """
  } applyRefactoring(renameTo("Override"))
  
  @Test
//  @Ignore
  def renameAnnotationOnImport = new FileSet {
    """
    package renameAnnotationOnImport
    
    import javax.xml.bind.annotation./*(*/XmlRootElement/*)*/
    
    @XmlRootElement
    class Element(x: String)
    """ becomes
    """
    package renameAnnotationOnImport
    
    import javax.xml.bind.annotation./*(*/Root/*)*/
    
    @XmlRootElement
    @Root
    class Element(x: String)
    """
  } applyRefactoring(renameTo("Root"))
  
  
}