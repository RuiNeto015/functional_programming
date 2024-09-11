package pj.domain.services

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.InvalidVivaJuryConstitution
import pj.domain.SimpleTypes.*
import pj.domain.services.IntervalService.hasIntersectionsConflicts
import pj.domain.{DomainError, TimeInterval}

class IntervalServiceTest extends AnyFunSuiteLike:
  test("hasIntersectionsConflicts is correct for TimeInterval 1 - spaced times"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:30:00")
      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:30:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:30:00")
      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:00:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)
      l <- Right(List(ti3, ti2, ti1))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(!hasIntersectionsConflicts(l))

  test("hasIntersectionsConflicts is correct for TimeInterval 2 - close times"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:30:00")
      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:31:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:30:00")
      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:31:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)
      l <- Right(List(ti3, ti2, ti1))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(!hasIntersectionsConflicts(l))

  test("hasIntersectionsConflicts is correct for TimeInterval 3 - list with one element"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      l <- Right(List(ti1))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(!hasIntersectionsConflicts(l))

  test("hasIntersectionsConflicts is correct for TimeInterval 3 - empty list"):
    assert(!hasIntersectionsConflicts(List()))

  test("hasIntersectionsConflicts is incorrect for TimeInterval 1 - conflicts on all"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T13:30:00")
      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:30:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:30:00")
      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T14:30:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)
      l <- Right(List(ti3, ti2, ti1))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(hasIntersectionsConflicts(l))

  test("hasIntersectionsConflicts is incorrect for TimeInterval 2 - conflicts on half"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")
      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")
      idt5 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:30:00")
      idt6 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T13:00:00")
      idt7 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T14:00:00")
      idt8 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T16:00:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)
      ti4 <- TimeInterval.from(idt7, idt8)
      l <- Right(List(ti3, ti2, ti1, ti4))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(hasIntersectionsConflicts(l))

  test("hasIntersectionsConflicts is incorrect for TimeInterval 2 - conflicts by seconds"):
    val result = for {
      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")
      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:59:59")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      l <- Right(List(ti1, ti2))
    } yield l
    result match
      case Left(_) => assert(false)
      case Right(l) => assert(hasIntersectionsConflicts(l))
