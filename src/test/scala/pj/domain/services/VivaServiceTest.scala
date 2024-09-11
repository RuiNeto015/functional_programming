package pj.domain.services

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.*
import pj.domain.DomainError.InvalidVivaJuryConstitution
import pj.domain.SimpleTypes.*
import pj.domain.services.VivaService.*

class VivaServiceTest extends AnyFunSuiteLike:

  test("supportsInterval is correct 1 - overlaps"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a,b)

    result match
      case Right(a) => assert(a)
      case Left(_) => assert(false)


  test("supportsInterval is correct 2 - contains"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a, b)

    result match
      case Right(a) => assert(a)
      case Left(_) => assert(false)

  test("supportsInterval is correct 3 - starts"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T15:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a, b)

    result match
      case Right(a) => assert(a)
      case Left(_) => assert(false)

  test("supportsInterval is correct 3 - equals"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a, b)

    result match
      case Right(a) => assert(a)
      case Left(_) => assert(false)

  test("supportsInterval is incorrect 1 - precedes"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T11:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a, b)

    result match
      case Right(a) => assert(!a)
      case Left(_) => assert(false)

  test("supportsInterval is incorrect 1 - meets"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidVivaJuryConstitution.apply)("2024-05-30T12:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield supportsInterval(a, b)

    result match
      case Right(a) => assert(!a)
      case Left(_) => assert(false)
