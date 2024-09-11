package pj.domain

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.{InvalidPreference, InvalidTimeInterval}
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, ResourceId, ResourceName, VivaDuration, VivaStudent, VivaTitle}
import pj.domain.xml.XMLToDomain

import scala.xml.Elem

class XMLToDomainTest extends AnyFunSuite:

  test("parseToDomain - parse valid XML correctly"):
    val xml: Elem =
        <agenda xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../agenda.xsd"
                duration="01:00:00">
          <vivas>
            <viva student="Student 001" title="Title 1">
              <president id="T001"/>
              <advisor id="T002"/>
              <supervisor id="E001"/>
            </viva>
          </vivas>
          <resources>
            <teachers>
              <teacher id="T001" name="Teacher 001">
                <availability start="2024-05-30T09:30:00" end="2024-05-30T10:30:00" preference="5"/>
                <availability start="2024-05-30T14:30:00" end="2024-05-30T15:30:00" preference="3"/>
              </teacher>
              <teacher id="T002" name="Teacher 002">
                <availability start="2024-05-30T09:30:00" end="2024-05-30T10:30:00" preference="5"/>
                <availability start="2024-05-30T14:30:00" end="2024-05-30T15:30:00" preference="3"/>
              </teacher>
            </teachers>
            <externals>
              <external id="E001" name="External 001">
                <availability start="2024-05-30T14:30:00" end="2024-05-30T15:30:00" preference="5"/>
              </external>
            </externals>
          </resources>
        </agenda>

    val a: Result[Agenda] = for {
      vd <- VivaDuration.from("01:00:00")

      t1idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t1idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t1idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t1idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t1inter1 <- TimeInterval.from(t1idt1, t1idt2)
      t1inter2 <- TimeInterval.from(t1idt3, t1idt4)
      t1p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t1p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t1a1 = AvailabilityAgenda(t1inter1, t1p1)
      t1a2 = AvailabilityAgenda(t1inter2, t1p2)
      t1i <- ResourceId.from("T001")
      t1n <- ResourceName.from("Teacher 001")
      t1 <- Teacher.from(t1i, t1n, List(t1a1, t1a2))

      t2idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t2idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t2idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t2idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t2inter1 <- TimeInterval.from(t2idt1, t2idt2)
      t2inter2 <- TimeInterval.from(t2idt3, t2idt4)
      t2p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t2p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t2a1 = AvailabilityAgenda(t2inter1, t2p1)
      t2a2 = AvailabilityAgenda(t2inter2, t2p2)
      t2i <- ResourceId.from("T002")
      t2n <- ResourceName.from("Teacher 002")
      t2 <- Teacher.from(t2i, t2n, List(t2a1, t2a2))

      e1idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      e1idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      e1inter1 <- TimeInterval.from(e1idt1, e1idt2)
      e1p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      e1a1 = AvailabilityAgenda(e1inter1, e1p1)
      e1i <- ResourceId.from("E001")
      e1n <- ResourceName.from("External 001")
      e1 <- External.from(e1i, e1n, List(e1a1))

      vj1 <- VivaJury.from(t1, t2, List(), List(e1))
      vs1 <- VivaStudent.from("Student 001")
      vt1 <- VivaTitle.from("Title 1")

      v1 = Viva(vt1, vs1, vj1)

      a <- Agenda.from(vd, List(v1))
    } yield a

    a match
      case Right(a1) =>
        val expected: Result[Agenda] = Right(a1)
        val actual: Result[Agenda] = XMLToDomain.parseToDomain(xml)
        assert(actual == expected)
      case _ => assert(false)


  test("parseToDomain - vivas are empty"):

    val a: Result[Agenda] = for {
      vd <- VivaDuration.from("01:00:00")
      a <- Agenda.from(vd, List())
    } yield a

    a match
      case Left(a1) => assert(true)
      case _ => assert(false)


  test("parseToDomain - duplicated student ids"):

    val a: Result[Agenda] = for {
      vd <- VivaDuration.from("01:00:00")

      t1idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t1idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t1idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t1idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t1inter1 <- TimeInterval.from(t1idt1, t1idt2)
      t1inter2 <- TimeInterval.from(t1idt3, t1idt4)
      t1p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t1p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t1a1 = AvailabilityAgenda(t1inter1, t1p1)
      t1a2 = AvailabilityAgenda(t1inter2, t1p2)
      t1i <- ResourceId.from("T001")
      t1n <- ResourceName.from("Teacher 001")
      t1 <- Teacher.from(t1i, t1n, List(t1a1, t1a2))

      t2idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t2idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t2idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t2idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t2inter1 <- TimeInterval.from(t2idt1, t2idt2)
      t2inter2 <- TimeInterval.from(t2idt3, t2idt4)
      t2p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t2p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t2a1 = AvailabilityAgenda(t2inter1, t2p1)
      t2a2 = AvailabilityAgenda(t2inter2, t2p2)
      t2i <- ResourceId.from("T002")
      t2n <- ResourceName.from("Teacher 002")
      t2 <- Teacher.from(t2i, t2n, List(t2a1, t2a2))

      vj1 <- VivaJury.from(t1, t2, List(), List())
      vs1 <- VivaStudent.from("Student 001")
      vt1 <- VivaTitle.from("Title 1")

      vj2 <- VivaJury.from(t1, t2, List(), List())
      vs2 <- VivaStudent.from("Student 001")
      vt2 <- VivaTitle.from("Title 2")

      v1 = Viva(vt1, vs1, vj1)
      v2 = Viva(vt2, vs2, vj2)

      a <- Agenda.from(vd, List(v1, v2))
    } yield a

    a match
      case Left(_) => assert(true)
      case _ => assert(false)


  test("parseToDomain - duplicated student title"):

    val a: Result[Agenda] = for {
      vd <- VivaDuration.from("01:00:00")

      t1idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t1idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t1idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t1idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t1inter1 <- TimeInterval.from(t1idt1, t1idt2)
      t1inter2 <- TimeInterval.from(t1idt3, t1idt4)
      t1p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t1p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t1a1 = AvailabilityAgenda(t1inter1, t1p1)
      t1a2 = AvailabilityAgenda(t1inter2, t1p2)
      t1i <- ResourceId.from("T001")
      t1n <- ResourceName.from("Teacher 001")
      t1 <- Teacher.from(t1i, t1n, List(t1a1, t1a2))

      t2idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      t2idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      t2idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:30:00")
      t2idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")
      t2inter1 <- TimeInterval.from(t2idt1, t2idt2)
      t2inter2 <- TimeInterval.from(t2idt3, t2idt4)
      t2p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
      t2p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      t2a1 = AvailabilityAgenda(t2inter1, t2p1)
      t2a2 = AvailabilityAgenda(t2inter2, t2p2)
      t2i <- ResourceId.from("T002")
      t2n <- ResourceName.from("Teacher 002")
      t2 <- Teacher.from(t2i, t2n, List(t2a1, t2a2))

      vj1 <- VivaJury.from(t1, t2, List(), List())
      vs1 <- VivaStudent.from("Student 001")
      vt1 <- VivaTitle.from("Title 1")

      vj2 <- VivaJury.from(t1, t2, List(), List())
      vs2 <- VivaStudent.from("Student 002")
      vt2 <- VivaTitle.from("Title 1")

      v1 = Viva(vt1, vs1, vj1)
      v2 = Viva(vt2, vs2, vj2)

      a <- Agenda.from(vd, List(v1, v2))
    } yield a

    a match
      case Left(_) => assert(true)
      case _ => assert(false)