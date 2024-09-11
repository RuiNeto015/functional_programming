package pj.domain.services

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe
import pj.domain.DomainError.{InvalidPreference, InvalidTimeInterval, InvalidVivaJuryConstitution}
import pj.domain.SimpleTypes.*
import pj.domain.services.ListService.*
import pj.domain.{AvailabilityAgenda, DomainError, External, Teacher, TimeInterval}

class ListServiceTest extends AnyFunSuiteLike:

  // Tests for sort

  test("sort is correct for TimeInterval 1 - spaced times"):
    val assertionResult = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:00:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T17:00:00")

      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      l <- Right(List(ti1, ti3, ti2))

      sort <- Right(ListService.sortList(l))

      assertion <- Right(sort shouldBe List(ti1, ti2, ti3))

    } yield assertion

    assertionResult match
      case Left(_) => assert(false)
      case Right(a) => a

  test("sort is correct for TimeInterval 2 - short times"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:01")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:01")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T17:00:00")

      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      l <- Right(List(ti3, ti1, ti2))

      sort <- Right(ListService.sortList(l))

      isCorrect <- Right(sort shouldBe List(ti1, ti2, ti3))

    } yield isCorrect

    result match
      case Left(_) => assert(false)
      case Right(a) => a

  test("sort is correct for TimeInterval 3"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:01")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T07:00:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T08:00:00")

      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      l <- Right(List(ti1, ti2, ti3))

      sort <- Right(ListService.sortList(l))

      isCorrect <- Right(sort shouldBe List(ti3, ti1, ti2))

    } yield isCorrect

    result match
      case Left(_) => assert(false)
      case Right(a) => a

  // Has duplicates

  test("has duplicates is correct 1"):
    assert(!hasDuplicates(List(1, 2, 3, 4, 5, 6)))

  test("has duplicates is correct 2 - external resources"):
    val result = for {
      resourceId <- ResourceId.from("E001")
      resourceName <- ResourceName.from("Name")
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      aa1 <- Right(AvailabilityAgenda(ti1, preference))

      e1 <- External.from(resourceId, resourceName, List(aa1))
      e2 <- External.from(resourceId, resourceName, List(aa1))

      hasDuplicates <- Right(hasDuplicates(List(e1, e2)))

    } yield hasDuplicates

    result match
      case Right(x) => assert(x)
      case Left(x) => assert(false)

  test("has duplicates is correct 2 - teacher resources"):
    val result = for {
      resourceId <- ResourceId.from("E001")
      resourceName <- ResourceName.from("Name")
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      aa1 <- Right(AvailabilityAgenda(ti1, preference))

      e1 <- Teacher.from(resourceId, resourceName, List(aa1))
      e2 <- Teacher.from(resourceId, resourceName, List(aa1))

      hasDuplicates <- Right(hasDuplicates(List(e1, e2)))

    } yield hasDuplicates

    result match
      case Right(x) => assert(x)
      case Left(x) => assert(false)

  test("not has duplicates is correct 1 -  resources"):
    val result = for {
      resourceId1 <- ResourceId.from("E001")
      resourceId2 <- ResourceId.from("E002")
      resourceName <- ResourceName.from("Name")
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      aa1 <- Right(AvailabilityAgenda(ti1, preference))

      e1 <- Teacher.from(resourceId1, resourceName, List(aa1))
      e2 <- Teacher.from(resourceId2, resourceName, List(aa1))

      hasDuplicates <- Right(hasDuplicates(List(e1, e2)))

    } yield hasDuplicates

    result match
      case Right(x) => assert(!x)
      case Left(x) => assert(false)

  // hasDuplicatedValue

  test("not has duplicated value 1 -  by preference"):
    val result = for {
      p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")
      p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("1")

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      aa1 <- Right(AvailabilityAgenda(ti1, p1))
      aa2 <- Right(AvailabilityAgenda(ti1, p2))

    } yield hasDuplicatedValue(List(aa1, aa2), _.preference)

    result match
      case Right(x) => assert(!x)
      case Left(x) => assert(false)

  test("has duplicated value 1 -  by preference"):
    val result = for {
      p1 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")
      p2 <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      ti1 <- TimeInterval.from(idt1, idt2)

      aa1 <- Right(AvailabilityAgenda(ti1, p1))
      aa2 <- Right(AvailabilityAgenda(ti1, p2))

    } yield hasDuplicatedValue(List(aa1, aa2), _.preference)

    result match
      case Right(x) => assert(x)
      case Left(x) => assert(false)
