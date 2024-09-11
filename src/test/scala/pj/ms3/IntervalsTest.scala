package pj.ms3

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.InvalidTimeInterval
import pj.domain.SimpleTypes.*
import pj.domain.schedule.ScheduleMS03.findDisputedIntervals
import pj.domain.{DomainError, TimeInterval, Viva}


class IntervalsTest extends AnyFunSuiteLike:

  /**
   * Given this list of time intervals, the result should be:
   * List((null, List(ti1)), (null, List(ti2)), (null, List(ti3, ti4)), (null, List(ti5))))
   * since the vivas must have the found "shorter" intervals that are inner any of its intervals
   */
  test("test case #1 - valid"):

    val result = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:00:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T12:00:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T12:00:00")
      ti5 <- TimeInterval.from(idt9, idt10)
    } yield
      findDisputedIntervals(List((null, List(ti1)), (null, List(ti2)), (null, List(ti3, ti4)), (null, List(ti5))))

    val expectedOutput = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:00:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      ti5 <- TimeInterval.from(idt9, idt10)

      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T09:50:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T10:00:00")
      ti6 <- TimeInterval.from(idt11, idt12)

      idt13 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      idt14 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T12:00:00")
      ti7 <- TimeInterval.from(idt13, idt14)

      idt15 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T11:00:00")
      idt16 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T12:00:00")
      ti8 <- TimeInterval.from(idt15, idt16)
    } yield
      List((null, List(ti1, ti2)), (null, List(ti3, ti4)), (null, List(ti5, ti6, ti7)), (null, List(ti8)))

    assert(result == expectedOutput)

  /**
   * Given this list of time intervals, the result should be:
   * List((null, List(ti1)), (null, List(ti2)), (null, List(ti3, ti4)), (null, List(ti5)), (null, List(ti6)),
   * (null, List(ti7)), (null, List(ti8)))
   * since the vivas must have the found "shorter" intervals that are inner any of its intervals
   */
  test("test case #2 - valid"):

    val result = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:20:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T18:40:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:40:00")
      ti5 <- TimeInterval.from(idt9, idt10)

      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:40:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:40:00")
      ti6 <- TimeInterval.from(idt11, idt12)

      idt13 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:30:00")
      idt14 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T21:30:00")
      ti7 <- TimeInterval.from(idt13, idt14)

      idt15 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:30:00")
      idt16 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T21:30:00")
      ti8 <- TimeInterval.from(idt15, idt16)
    } yield
      findDisputedIntervals(List((null, List(ti1)), (null, List(ti2)), (null, List(ti3, ti4)), (null, List(ti5)),
        (null, List(ti6)), (null, List(ti7)), (null, List(ti8))))

    val expectedOutput = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:20:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:20:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:40:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:40:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T18:40:00")
      ti5 <- TimeInterval.from(idt9, idt10)

      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T18:40:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:30:00")
      ti6 <- TimeInterval.from(idt11, idt12)

      idt13 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:30:00")
      idt14 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:40:00")
      ti7 <- TimeInterval.from(idt13, idt14)

      idt15 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:40:00")
      idt16 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T21:30:00")
      ti8 <- TimeInterval.from(idt15, idt16)
    } yield
      List(
        (null, List(ti1, ti2)), (null, List(ti2, ti3)), (null, List(ti1, ti2, ti4, ti5)),
        (null, List(ti1, ti2, ti3, ti4, ti5, ti6, ti7)), (null, List(ti5, ti6, ti7)),
        (null, List(ti7, ti8)), (null, List(ti7, ti8)))

    assert(result == expectedOutput)

  /**
   * Given this list of time intervals, the result should be:
   * false
   * since the vivas do not have the valid "shorter" intervals
   */
  test("test case #3 - invalid"):

    val result = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:20:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T17:30:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T18:40:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T15:30:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T19:40:00")
      ti5 <- TimeInterval.from(idt9, idt10)
    } yield
      findDisputedIntervals(List((null, List(ti1)), (null, List(ti2)), (null, List(ti3, ti4)), (null, List(ti5))))

    val expectedOutput = for {
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:50:00")
      ti1 <- TimeInterval.from(idt1, idt2)

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:20:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T18:30:00")
      ti2 <- TimeInterval.from(idt3, idt4)

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T16:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T20:30:00")
      ti3 <- TimeInterval.from(idt5, idt6)

      idt7 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T21:30:00")
      idt8 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T21:40:00")
      ti4 <- TimeInterval.from(idt7, idt8)

      idt9 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T22:40:00")
      idt10 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2025-05-20T23:40:00")
      ti5 <- TimeInterval.from(idt9, idt10)
    } yield
      List(
        (null, List(ti1, ti2)), (null, List(ti2, ti3)), (null, List(ti1, ti2, ti4, ti5)))

    assert(result != expectedOutput)



