package pj.domain

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.SimpleTypes.*
import pj.domain.DomainError.*

class VivaJuryTest extends AnyFunSuiteLike:

  test("Viva jury is correct with 1 of each and 2 coAdvisors"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      president     <- Teacher.from(teacher1id, teacher1name, List(availability))

      teacher2id    <- ResourceId.from("T002")
      teacher2name  <- ResourceName.from("Teacher 002")
      advisor       <- Teacher.from(teacher2id, teacher2name, List(availability))

      external1id   <- ResourceId.from("E001")
      external1name <- ResourceName.from("External 001")
      coadvisorE    <- External.from(external1id, external1name, List(availability))

      teacher3id    <- ResourceId.from("T003")
      teacher3name  <- ResourceName.from("Teacher 003")
      coadvisorT    <- Teacher.from(teacher3id, teacher3name, List(availability))

      external2id   <- ResourceId.from("E002")
      external2name <- ResourceName.from("External 002")
      supervisor    <- External.from(external2id, external2name, List(availability))

      vj            <- VivaJury.from(president, advisor, List(coadvisorE, coadvisorT), List(supervisor))
    } yield vj

    result match
      case Left(_) => assert(false)
      case Right(vj) => assert(true)


  test("Viva jury is correct with empty lists"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      president     <- Teacher.from(teacher1id, teacher1name, List(availability))

      teacher2id    <- ResourceId.from("T002")
      teacher2name  <- ResourceName.from("Teacher 002")
      advisor       <- Teacher.from(teacher2id, teacher2name, List(availability))
      vj            <- VivaJury.from(president, advisor, List(), List())
    } yield vj

    result match
      case Left(_) => assert(false)
      case Right(vj) => assert(true)


  test("Viva jury is correct with 2 on each list of each type"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      president     <- Teacher.from(teacher1id, teacher1name, List(availability))

      t2id          <- ResourceId.from("T002")
      t2name        <- ResourceName.from("Teacher 002")
      advisor       <- Teacher.from(t2id, t2name, List(availability))

      e1id          <- ResourceId.from("E001")
      e1name        <- ResourceName.from("External 001")
      coadvisorE1   <- External.from(e1id, e1name, List(availability))

      t3id          <- ResourceId.from("T003")
      t3name        <- ResourceName.from("Teacher 003")
      coadvisorT3   <- Teacher.from(t3id, t3name, List(availability))

      e2id          <- ResourceId.from("E002")
      e2name        <- ResourceName.from("External 002")
      supervisorE2  <- External.from(e2id, e2name, List(availability))

      e3id          <- ResourceId.from("E003")
      e3name        <- ResourceName.from("External 003")
      supervisorE3  <- External.from(e3id, e3name, List(availability))

      vj            <- VivaJury.from(president, advisor, List(coadvisorE1, coadvisorT3), List(supervisorE2, supervisorE3))
    } yield vj

    result match
      case Left(_) => assert(false)
      case Right(vj) => assert(true)


  test("Viva jury is incorrect with president and advisor being equals, and empty list"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      teacher1      <- Teacher.from(teacher1id, teacher1name, List(availability))

      vj            <- VivaJury.from(teacher1, teacher1, List(), List())
    } yield vj

    result match
      case Left(_) => assert(true)
      case Right(vj) => assert(false)


  test("Viva jury is incorrect coAdvisors repeated"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      teacher1      <- Teacher.from(teacher1id, teacher1name, List(availability))

      e1id          <- ResourceId.from("E001")
      e1name        <- ResourceName.from("External 001")
      coadvisorE1   <- External.from(e1id, e1name, List(availability))

      vj            <- VivaJury.from(teacher1, teacher1, List(coadvisorE1, coadvisorE1), List())
    } yield vj

    result match
      case Left(_) => assert(true)
      case Right(vj) => assert(false)


  test("Viva jury is incorrect supervisors repeated"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      teacher1      <- Teacher.from(teacher1id, teacher1name, List(availability))

      e3id          <- ResourceId.from("E003")
      e3name        <- ResourceName.from("External 003")
      supervisorE3  <- External.from(e3id, e3name, List(availability))

      vj            <- VivaJury.from(teacher1, teacher1, List(), List(supervisorE3, supervisorE3))
    } yield vj

    result match
      case Left(_) => assert(true)
      case Right(vj) => assert(false)


  test("Viva jury is incorrect with coAdvisors and supervisors sharing same resource"):
    val result = for {
      preference    <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start         <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end           <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval  <- TimeInterval.from(start, end)
      availability  <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id    <- ResourceId.from("T001")
      teacher1name  <- ResourceName.from("Teacher 001")
      teacher1      <- Teacher.from(teacher1id, teacher1name, List(availability))

      e1id          <- ResourceId.from("E001")
      e1name        <- ResourceName.from("External 001")
      externalE1    <- External.from(e1id, e1name, List(availability))

      vj            <- VivaJury.from(teacher1, teacher1, List(externalE1), List(externalE1))
    } yield vj

    result match
      case Left(_) => assert(true)
      case Right(vj) => assert(false)
