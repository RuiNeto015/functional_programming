package pj.domain.services

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain
import pj.domain.DomainError.InvalidTimeInterval
import pj.domain.SimpleTypes.IntervalDateTime
import pj.domain.TimeInterval
import pj.domain.services.IntervalService.*

class IntervalAlgebraTest extends AnyFunSuiteLike:

  test("Precedes is correct 1"):

    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precedes(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Precedes is correct 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:01")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precedes(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Precedes is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precedes(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Precedes is incorrect 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precedes(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Meets is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield meets(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Meets is correct 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:39:59")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:39:59")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield meets(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Meets is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield meets(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Meets is incorrect 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:01")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield meets(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Overlaps is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield overlaps(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Overlaps is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield overlaps(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("FinishedBy is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield finishedBy(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("FinishedBy is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:01:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield finishedBy(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Contains is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield contains(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Contains is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield contains(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Starts is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T13:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield starts(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Starts is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield starts(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Equals is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield IntervalService.equals(a, b)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Equals is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:01")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield IntervalService.equals(a, b)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("PrecededBy is correct 1"):

    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precededBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("PrecededBy is correct 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:01")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precededBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("PrecededBy is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precededBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("PrecededBy is incorrect 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield precededBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("MetBy is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield metBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("MetBy is correct 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:39:59")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:39:59")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield metBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("MetBy is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield metBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("MetBy is incorrect 2"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:01")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield metBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("OverlappedBy is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield overlappedBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("OverlappedBy is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield overlappedBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("Finishes is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield finishes(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("Finishes is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:01:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield finishes(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("During is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield during(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("During is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T14:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield during(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)

  test("StartedBy is correct 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T13:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield startedBy(b, a)

    result match
      case Right(x) => assert(x)
      case Left(_) => assert(false)

  test("StartedBy is incorrect 1"):
    val result = for {

      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:00:00")

      a <- TimeInterval.from(idt1, idt2)
      b <- TimeInterval.from(idt3, idt4)

    } yield startedBy(b, a)

    result match
      case Right(x) => assert(!x)
      case Left(_) => assert(false)