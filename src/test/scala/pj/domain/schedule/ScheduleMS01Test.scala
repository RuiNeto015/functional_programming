package pj.domain.schedule

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError
import pj.domain.DomainError.ImpossibleSchedule
import pj.io.FileIO.*

import java.io.File
import scala.language.adhocExtensions
import scala.xml.{Elem, Utility}


class ScheduleMS01Test extends AnyFunSuite:

  val PATH = "files/test/ms01/"

  /**
   * Execute the logic to test a valid schedule
   *
   * @param filename the input filename
   * @return test's assertion
   */
  def execValid(filename: String): Assertion =
    val ip = PATH + filename + "_in.xml"
    val op = PATH + filename + "_out.xml"

    val r = for {
      ixml <- load(new File(ip)) // load input file
      ogxml <- load(new File(op)) // load the expected output file
      oxml <- ScheduleMS01.create(ixml) // convert input file into generator output
    } yield (oxml, ogxml)

    r match
      case Right(o) => assert(Utility.trim(o._1) == Utility.trim(o._2))
      case Left(e) => fail(s"get error: $e")

  /**
   * Execute the logic to test a invalid schedule
   *
   * @param filename the input filename
   * @return test's assertion
   */
  def execInvalid(filename: String): Assertion =
    val ip = PATH + filename + "_in.xml"

    val r = for {
      ixml <- load(new File(ip))
      oxml <- ScheduleMS01.create(ixml)
    } yield oxml

    r match
      case Right(o) => fail("generated schedule but the goal it was to fail")
      case Left(e: ImpossibleSchedule.type) => assert(true)
      case Left(e2) => fail(s"get other error instead ImpossibleSchedule : $e2")


  test("valid schedule 1"):
    execValid("valid_sample01")

  test("valid schedule 2"):
    execValid("valid_sample02")

  test("valid schedule 3"):
    execValid("valid_sample03")

  test("invalid schedule 1"):
    execInvalid("invalid_sample01")

  test("invalid schedule 2"):
    execInvalid("invalid_sample02")

  test("invalid schedule 3"):
    execInvalid("invalid_sample03")