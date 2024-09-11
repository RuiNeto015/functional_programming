package pj.ms3

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.InvalidTimeInterval
import pj.domain.SimpleTypes.{IntervalDateTime, VivaDuration}
import pj.domain.{Result, TimeInterval, Viva}
import pj.domain.schedule.ScheduleMS03.intraVivaCombine

class IntraCombineTest extends AnyFunSuite:

  /**
   * Given a viva (duration = 1h) that can occupy the following intervals:
   *
   *     (9h40)     (10h30)
   * int0  |-----------|        (11h30)
   * int1              |-----------| (11h40)    (12h40)
   * int2                             |-----------|
   *
   * all the possible combinations of intervals that this viva can occupy are [ [int0, int1], [int1], [int2] ]
   */
  test("test case #1"):
    val result: Result[Boolean] = for {
      idt01 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:40:00")
      idt02 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      int0 <- TimeInterval.from(idt01, idt02)
      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:30:00")
      int1 <- TimeInterval.from(idt11, idt12)
      idt21 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:40:00")
      idt22 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:40:00")
      int2 <- TimeInterval.from(idt21, idt22)
      duration <- VivaDuration.from("01:00:00")
    } yield
      val combinations = intraVivaCombine(List((null, List(int0, int1, int2))), duration)
      combinations.headOption.fold(false)(tuple => tuple._2.equals(List(List(int0, int1), List(int1), List(int2))))

    assert(result.fold(_ => false, r => r))

  /**
   * Given a viva (duration = 1h) that can occupy the following intervals:
   *
   *     (9h40)     (10h30)
   * int0  |-----------| (10h40)    (11h30)
   * int1                  |-----------|        (12h30)
   * int2                              |-----------|
   *
   * all the possible combinations of intervals that this viva can occupy are [ [int1, int2], [int2] ]
   */
  test("test case #2"):
    val result: Result[Boolean] = for {
      idt01 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:40:00")
      idt02 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      int0 <- TimeInterval.from(idt01, idt02)
      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:30:00")
      int1 <- TimeInterval.from(idt11, idt12)
      idt21 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:30:00")
      idt22 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:30:00")
      int2 <- TimeInterval.from(idt21, idt22)
      duration <- VivaDuration.from("01:00:00")
    } yield
      val combinations = intraVivaCombine(List((null, List(int0, int1, int2))), duration)
      combinations.headOption.fold(false)(tuple => tuple._2.equals(List(List(int1, int2), List(int2))))

    assert(result.fold(_ => false, r => r))

  /**
   * Given a viva (duration = 1h) that can occupy the following intervals:
   *
   *     (9h40)     (10h30)
   * int0  |-----------| (10h40)    (11h30)
   * int1                  |-----------| (11h40)    (12h30)
   * int2                                  |-----------|
   *
   * all the possible combinations of intervals that this viva can occupy are []
   */
  test("test case #3"):
    val result: Result[Boolean] = for {
      idt01 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:40:00")
      idt02 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      int0 <- TimeInterval.from(idt01, idt02)
      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:40:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:30:00")
      int1 <- TimeInterval.from(idt11, idt12)
      idt21 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:40:00")
      idt22 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:30:00")
      int2 <- TimeInterval.from(idt21, idt22)
      duration <- VivaDuration.from("01:00:00")
    } yield
      val combinations = intraVivaCombine(List((null, List(int0, int1, int2))), duration)
      combinations.headOption.fold(false)(tuple => tuple._2.equals(List()))

    assert(result.fold(_ => false, r => r))

  /**
   * Given a viva (duration = 1h) that can occupy the following intervals:
   *
   *     (9h40)      (10h30)
   * int0  |-----------|        (11h40)
   * int1              |-----------|       (12h40)
   * int2                          |-----------|
   *
   * all the possible combinations of intervals that this viva can occupy are [ [int0, int1], [int0, int1, int2],
   * [int1], [int1, int2], [int2] ]
   */
  test("test case #4"):
    val result: Result[Boolean] = for {
      idt01 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:40:00")
      idt02 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      int0 <- TimeInterval.from(idt01, idt02)
      idt11 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")
      idt12 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:40:00")
      int1 <- TimeInterval.from(idt11, idt12)
      idt21 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T11:40:00")
      idt22 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:40:00")
      int2 <- TimeInterval.from(idt21, idt22)
      duration <- VivaDuration.from("01:00:00")
    } yield
      val combinations = intraVivaCombine(List((null, List(int0, int1, int2))), duration)
      combinations.headOption.fold(false)(tuple =>
        tuple._2.equals(List(List(int0, int1), List(int0, int1, int2), List(int1), List(int1, int2), List(int2))))

    assert(result.fold(_ => false, r => r))