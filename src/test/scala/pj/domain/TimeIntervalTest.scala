package pj.domain

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.SimpleTypes.*
import pj.domain.DomainError.*

class TimeIntervalTest extends AnyFunSuiteLike:

  test("TimeInterval is correct - start is before end"):
    val result = for {
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T11:00:00")
      interval <- TimeInterval.from(start, end)
    } yield interval

    result match
      case Right(interval) => assert(true)
      case Left(_) => assert(false)


  test("TimeInterval is incorrect - start is not before end"):
    val result = for {
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T11:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      interval <- TimeInterval.from(start, end)
    } yield interval

    result match
      case Right(interval) => assert(false)
      case Left(_) => assert(true)


  test("TimeInterval is incorrect - start and end are the same"):
    val result = for {
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      interval <- TimeInterval.from(start, end)
    } yield interval

    result match
      case Right(interval) => assert(false)
      case Left(_) => assert(true)


  test("TimeInterval is correct - start and end are on different days"):
    val result = for {
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T10:00:00")
      interval <- TimeInterval.from(start, end)
    } yield interval

    result match
      case Right(interval) => assert(true)
      case Left(_) => assert(false)


  test("TimeInterval is incorrect - start and end are on the same time but different days"):
    val result = for {
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-01T10:00:00")
      interval <- TimeInterval.from(start, end)
    } yield interval

    result match
      case Right(interval) => assert(false)
      case Left(_) => assert(true)
