package pj.domain

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.XMLError
import pj.domain.SimpleTypes.*
import pj.domain.xml.DomainToXML

import java.time.LocalDateTime
import scala.xml.Utility

class DomainToXMLTest extends AnyFunSuiteLike:

  test("XMLWriter of schedule is correct"):
    val result = for {
      av <- AvailabilityAgendaPreference.from(XMLError.apply)("1")

      // Time Intervals
      idt1 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T14:00:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      idtr1 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T09:00:00")
      idtr2 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T10:00:00")

      idtr3 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T11:00:00")
      idtr4 <- IntervalDateTime.from(XMLError.apply)("2024-05-30T12:00:00")

      tir1 <- TimeInterval.from(idtr1, idtr2)
      tir2 <- TimeInterval.from(idtr3, idtr4)

      // Teacher
      rId1 <- ResourceId.from("T001")
      rName1 <- ResourceName.from("Teacher 1")
      avA1 <- Right(AvailabilityAgenda(ti1, av))
      t1 <- Teacher.from(rId1, rName1, List(avA1))

      rId2 <- ResourceId.from("T002")
      rName2 <- ResourceName.from("Teacher 2")
      avA2 <- Right(AvailabilityAgenda(ti1, av))
      t2 <- Teacher.from(rId2, rName2, List(avA2))

      // Externals
      rId3 <- ResourceId.from("E001")
      rName3 <- ResourceName.from("External 1")
      avA3 <- Right(AvailabilityAgenda(ti1, av))
      e1 <- External.from(rId3, rName3, List(avA3))

      rId4 <- ResourceId.from("E002")
      rName4 <- ResourceName.from("External 2")
      avA4 <- Right(AvailabilityAgenda(ti1, av))
      e2 <- External.from(rId4, rName4, List(avA4))

      rId5 <- ResourceId.from("E003")
      rName5 <- ResourceName.from("External 3")
      avA5 <- Right(AvailabilityAgenda(ti1, av))
      e3 <- External.from(rId5, rName5, List(avA5))

      // Viva Jury
      vj1 <- VivaJury.from(t1, t2, List(), List(e1))
      vj2 <- VivaJury.from(t1, t2, List(e2, e3), List())

      // Viva
      vt1 <- VivaTitle.from("Viva 1")
      vs1 <- VivaStudent.from("Student 1")
      v1 <- Right(Viva(vt1, vs1, vj1))

      vt2 <- VivaTitle.from("Viva 2")
      vs2 <- VivaStudent.from("Student 2")
      v2 <- Right(Viva(vt2, vs2, vj2))

      // ScheduleViva
      auxA <- AvailabilityAgendaPreference.from(XMLError.apply)("5")
      pref <- Right(SchedulePreference.zero + auxA)

      as1 <- Right(AvailabilitySchedule(tir1, pref))
      as2 <- Right(AvailabilitySchedule(tir2, pref))

      s <- Schedule.from(List(ScheduleViva(v1, as1), ScheduleViva(v2, as2)), pref)
    } yield s

    result match
      case Left(x) => assert(false)
      case Right(r) =>
        val excepted =
          <schedule xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference="5"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <viva student="Student 1" title="Viva 1" start="2024-05-30T09:00:00" end="2024-05-30T10:00:00" preference="5">
              <president name="Teacher 1"/>
              <advisor name="Teacher 2"/>
              <supervisor name="External 1"/>
            </viva>
            <viva student="Student 2" title="Viva 2" start="2024-05-30T11:00:00" end="2024-05-30T12:00:00" preference="5">
              <president name="Teacher 1"/>
              <advisor name="Teacher 2"/>
              <coadvisor name="External 2"/>
              <coadvisor name="External 3"/>
            </viva>
          </schedule>

        val out = DomainToXML.scheduleToXml("../../schedule.xsd")(r)
        assert(Utility.trim(excepted) == Utility.trim(out))

  test("XMLWriter of error is correct"):
    val excepted = <error xsi:noNamespaceSchemaLocation="../../schedule.xsd" message="XMLError(error)" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>
    val out = DomainToXML.errorToXml("../../schedule.xsd")(XMLError("error"))
    assert(Utility.trim(excepted) == Utility.trim(out))

