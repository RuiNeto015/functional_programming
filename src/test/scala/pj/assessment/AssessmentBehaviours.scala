package pj.assessment

import scala.language.adhocExtensions

import java.io.File

import org.scalatest.funsuite.AnyFunSuite

import scala.xml.{Elem, Utility}

import pj.io.FileIO.*
import pj.domain.{DomainError, Result }
import pj.domain.DomainError.IOFileProblem


trait AssessmentBehaviours extends AnyFunSuite:
  val IN = "_in.xml"              // Input file termination
  val OUT = "_out.xml"            // Output file termination
  val OUTERROR = "_outError.xml"  // Error file termination
  val OUTDIFF = "_outDiff.xml"    // Generated file termination (only written when xml files do not match)
  def PATH: String                // Assessment file path
  val OK = "OK"
  val FAILURE_ERRORFILE = "XML ERROR FILE EXPECTED AND NOT FOUND!"
  val NOMATCH = "XML FILES DID NOT MATCH!"

  private def validateError(f: File, e: DomainError): (File, String) = e match
    case IOFileProblem(message) => (f, message)
    case other                  => (f, other.toString)

  private def testWithFailure(d: DomainError, f: File): (File, String) =
    val efn = f.getPath.replace(IN, OUTERROR)
    loadError(efn).fold(_ => (f, FAILURE_ERRORFILE), s => if (s == d.toString) (f, OK) else (f, s"Expected: $s ; Result: $d") )

  private def testWithSuccess(xml: Elem, f: File): (File, String) =
    val ofn = f.getPath.replace(IN, OUT)
    load(ofn).fold(
      e => validateError(f,e),
      xmlf => if (Utility.trim(xml) == Utility.trim(xmlf)) (f, OK) else
        save(f.getPath.replace(IN, OUTDIFF), xml)
        (f, NOMATCH)
    )

  private def testFile(f: File, ms: Elem => Result[Elem]): (File, String) =
    val tout = for
      ixml <- load(f)   // load input file
      oxml <- ms(ixml)  // convert input file into output file
    yield oxml
    tout.fold(d => testWithFailure(d,f), xml => testWithSuccess(xml, f))

  def performTests(ms: Elem => Result[Elem], milestone : String): Unit =
    test("Assessment of " + milestone):
      val testInputFiles = new File(PATH).listFiles(_.getName.endsWith(IN))
      val numTests = testInputFiles.length
      val tested = testInputFiles.map(testFile(_, ms))
      tested.foreach { case (f, t) => println(s"File: ${f.getName} Result: $t") }
      val passedTests = tested.count { case (_, t) => t == OK }
      val message : String = if (numTests == 0) s"There were no tests for $milestone!"
        else
          val ratio: Int = (passedTests*100) / numTests
          s"Final score of $milestone: $passedTests / ${testInputFiles.size} = $ratio"
      println(message)
      assert( passedTests == numTests )
