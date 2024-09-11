package pj.domain.services

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.{InvalidPreference, InvalidTimeInterval}
import pj.domain.SimpleTypes.*
import pj.domain.*

class ScheduleServiceTest extends AnyFunSuiteLike:

  test("rebuildAvailabilitiesMap is correct - before and after - invalid vivas (duration doesn't fit into)"):

    val rid1 = for {
      rid <- ResourceId.from("T001")
    } yield rid

    val aa1 = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T10:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      aap1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
    } yield AvailabilityAgenda(ti1, aap1)

    val aa2 = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T14:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T17:00:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      aap1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
    } yield AvailabilityAgenda(ti1, aap1)

    val aa3 = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-20T10:00:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      aap1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
    } yield AvailabilityAgenda(ti1, aap1)

    val vj = for {
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval <- TimeInterval.from(start, end)
      availability <- Right(AvailabilityAgenda(timeInterval, preference))

      teacher1id <- ResourceId.from("T001")
      teacher1name <- ResourceName.from("Teacher 001")
      president <- Teacher.from(teacher1id, teacher1name, List(availability))

      t2id <- ResourceId.from("T002")
      t2name <- ResourceName.from("Teacher 002")
      advisor <- Teacher.from(t2id, t2name, List(availability))

      vj <- VivaJury.from(president, advisor, List(), List())
    } yield vj

    val duration = VivaDuration.from("01:00:00")
