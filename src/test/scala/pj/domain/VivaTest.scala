package pj.domain

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.{InvalidPreference, InvalidTimeInterval, InvalidVivaJuryConstitution,
  InvalidVivaStudent, InvalidVivaTitle}
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, ResourceId, ResourceName, VivaStudent,
  VivaTitle}


class VivaTest extends AnyFunSuiteLike:

  val vj: Result[VivaJury] = for {
    p <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
    s <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
    e <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
    ti <- TimeInterval.from(s, e)
    aa = AvailabilityAgenda(ti, p)
    r1 <- ResourceId.from("T001")
    r2 <- ResourceId.from("T002")
    rn1 <- ResourceName.from("Name")
    rn2 <- ResourceName.from("Name")
    t1 <- Teacher.from(r1, rn1, List(aa))
    t2 <- Teacher.from(r2, rn2, List(aa))
    vj <- VivaJury.from(t1, t2, List(), List())
  } yield vj

  val vjd: Result[VivaJury] = for {
    p <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
    s <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
    e <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
    ti <- TimeInterval.from(s, e)
    aa = AvailabilityAgenda(ti, p)
    r1 <- ResourceId.from("T001")
    rn1 <- ResourceName.from("Name")
    rn2 <- ResourceName.from("Name")
    t1 <- Teacher.from(r1, rn1, List(aa))
    t2 <- Teacher.from(r1, rn2, List(aa))
    vj <- VivaJury.from(t1, t2, List(), List())
  } yield vj


  test("Viva is correct 1"):
    val title = VivaTitle.from("Title 1")
    val student = VivaStudent.from("Student")
    (title, student, vj) match
      case (Right(t), Right(s), Right(vj1)) => assert(true)
      case _ => assert(false)


  test("Viva is incorrect with title error"):
    val title = VivaTitle.from("")
    val student = VivaStudent.from("Student")
    (title, student, vj) match
      case (Left(_), Right(s), Right(vj1)) =>
        assert(true)
      case _ => assert(false)


  test("Viva is incorrect with student error"):
    val title = VivaTitle.from("Title 1")
    val student = VivaStudent.from("")
    (title, student, vj) match
      case (Right(t), Left(_), Right(vj1)) =>
        assert(true)
      case _ => assert(false)


  test("Viva is incorrect with jury error"):
    val title = VivaTitle.from("Title 1")
    val student = VivaStudent.from("Student")
    (title, student, vjd) match
      case (Right(t), Right(s), Left(_)) =>
        assert(true)
      case _ => assert(false)


  test("Viva is incorrect with errors on everything"):
    val title = VivaTitle.from("")
    val student = VivaStudent.from("")
    val vjError = vjd
    (title, student, vjError) match
      case (Left(_), Left(_), Left(_)) => assert(true)
      case _ => assert(false)
