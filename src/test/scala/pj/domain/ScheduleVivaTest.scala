package pj.domain

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.InvalidPreference
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, VivaDuration}
import pj.domain.schedule.ScheduleMS01.scheduleViva
import pj.domain.services.{IntervalService, ScheduleService}

class ScheduleVivaTest extends AnyFunSuite:
  test("Schedule Viva is correct"):
    val listA = List[AvailabilityAgenda]()
    val listB = List[AvailabilityAgenda]()

    val a = for {
      i1 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      i2 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T12:00:00")
      ta <- TimeInterval.from(i1, i2)
      pa <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
      i3 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T12:00:00")
      i4 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T14:00:00")
      tb <- TimeInterval.from(i3, i4)
      pb <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
      i5 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T16:00:00")
      i6 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T17:00:00")
      tc <- TimeInterval.from(i5, i6)
      pc <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
    } yield AvailabilityAgenda(ta, pa) +: AvailabilityAgenda(tb, pb) +: AvailabilityAgenda(tc, pc) +: listA

    val b = for {
      i1 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T07:45:00")
      i2 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T11:00:00")
      ta <- TimeInterval.from(i1, i2)
      pa <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
      i3 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T12:00:00")
      i4 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T14:00:00")
      tb <- TimeInterval.from(i3, i4)
      pb <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
      i5 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T16:00:00")
      i6 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T16:30:00")
      tc <- TimeInterval.from(i5, i6)
      pc <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
      i7 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T18:00:00")
      i8 <- IntervalDateTime.from(DomainError.InvalidTimeInterval.apply)("2024-05-30T19:30:00")
      td <- TimeInterval.from(i7, i8)
      pd <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("4")
    } yield AvailabilityAgenda(ta, pa) +: AvailabilityAgenda(tb, pb) +: AvailabilityAgenda(tc, pc) +: AvailabilityAgenda(td, pd) +: listB

    for {
      listA <- a
      listB <- b
      duration <- VivaDuration.from("01:00:00")
    } yield {
      println(listA)
      println(listB)
      print(scheduleViva(List(listA, listB), duration))
    }