package pj.domain

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.DomainError.*
import pj.domain.SimpleTypes.*

class SimpleTypesTest extends AnyFunSuiteLike:

  test("valid resource id for teacher"):
    val resourceId = ResourceId.from("T123")
    assert(resourceId.fold(_ => false, _ => true))


  test("valid resource id for external"):
    val resourceId = ResourceId.from("E123")
    assert(resourceId.fold(_ => false, _ => true))


  test("invalid resource id"):
    val id1 = "TE123"
    val id2 = ""
    val id3 = "T1234"
    val id4 = "T12"

    assert(List(ResourceId.from(id1), ResourceId.from(id2), ResourceId.from(id3), ResourceId.from(id4))
      .count(x => x.fold(_ => true, _ => false)) == 4)


  test("valid resource name"):
    val resourceName = ResourceName.from("Teacher 001")
    assert(resourceName.fold(_ => false, _ => true))


  test("invalid resource name"):
    val resourceName = ResourceName.from("")
    assert(!resourceName.fold(_ => false, _ => true))


  test("valid IntervalDateTime from string"):
    val dateTimeStr = "2022-01-01T10:00:00"
    val intervalDateTime = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr)
    assert(intervalDateTime.fold(_ => false, _ => true))


  test("invalid IntervalDateTime from string"):
    val dateTimeStr = "invalid-date-time"
    val intervalDateTime = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr)
    assert(!intervalDateTime.fold(_ => false, _ => true))


  test("IntervalDateTime isBefore"):
    val dateTimeStr1 = "2022-01-01T10:00:00"
    val dateTimeStr2 = "2022-01-01T11:00:00"
    val intervalDateTime1 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr1)
    val intervalDateTime2 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr2)
    for {
      idt1 <- intervalDateTime1
      idt2 <- intervalDateTime2
    } yield assert(idt1.isBefore(idt2))


  test("IntervalDateTime isEqual"):
    val dateTimeStr = "2022-01-01T10:00:00"
    val intervalDateTime1 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr)
    val intervalDateTime2 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr)
    for {
      idt1 <- intervalDateTime1
      idt2 <- intervalDateTime2
    } yield assert(idt1.isEqual(idt2))


  test("IntervalDateTime isAfter"):
    val dateTimeStr1 = "2022-01-01T11:00:00"
    val dateTimeStr2 = "2022-01-01T10:00:00"
    val intervalDateTime1 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr1)
    val intervalDateTime2 = IntervalDateTime.from(InvalidTimeInterval.apply)(dateTimeStr2)
    for {
      idt1 <- intervalDateTime1
      idt2 <- intervalDateTime2
    } yield assert(idt1.isAfter(idt2))
    

  test("valid availability preference"):
    val p1 = AvailabilityAgendaPreference.from(InvalidPreference.apply)("1")
    val p2 = AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
    assert(List(p1, p2).count(x => x.fold(_ => false, _ => true)) == 2)


  test("invalid availability preference"):
    val p1 = AvailabilityAgendaPreference.from(InvalidPreference.apply)("0")
    val p2 = AvailabilityAgendaPreference.from(InvalidPreference.apply)("6")
    assert(List(p1, p2).count(x => x.fold(_ => true, _ => false)) == 2)


  test("valid viva title"):
    val t = VivaTitle.from("Title 1")
    assert(t.fold(_ => false, _ => true))


  test("invalid viva title"):
    val t = VivaTitle.from("")
    assert(!t.fold(_ => false, _ => true))


  test("valid viva student"):
    val s = VivaStudent.from("Student 001")
    assert(s.fold(_ => false, _ => true))


  test("invalid viva student"):
    val s = VivaStudent.from("")
    assert(!s.fold(_ => false, _ => true))


  test("SchedulePreference zero value"):
    assert(SchedulePreference.zero.equals(0))

  test("SchedulePreference addition with SchedulePreference"):
    val sp1: SchedulePreference = SchedulePreference.zero
    val sp2: SchedulePreference = SchedulePreference.zero
    assert((sp1 + sp2).equals(0))
    

  test("SchedulePreference addition with AvailabilityAgendaPreference"):
    val sp = SchedulePreference.zero
    val aap = AvailabilityAgendaPreference.from(InvalidPreference.apply)("5")
    aap match
      case Right(x) =>
        assert((sp + x).equals(5))
      case _ => Left(InvalidPreference)


  test("VivaDuration is correct 1"):
    val vd = VivaDuration.from("01:00:30")
      .fold(e => Left(InvalidVivaDuration("Invalid VivaDuration in tests")), vd => Right(vd))
    vd match
      case Right(vd1) =>
        assert(vd1.toString == "01:00:30")
      case _ => Left("Test VivaDuration is correct 1 failed")


  test("VivaDuration is correct 2"):
    val vd = VivaDuration.from("02:30:30")
      .fold(e => Left(InvalidVivaDuration("Invalid VivaDuration in tests")), vd => Right(vd))
    vd match
      case Right(vd1) =>
        assert(vd1.toString.equals("02:30:30"))
      case _ => Left("Test VivaDuration is correct 2 failed")


  test("VivaDuration is incorrect"):
    val vd = VivaDuration.from("25:00:00")
      .fold(e => Left(InvalidVivaDuration("Invalid VivaDuration in tests")), vd => Right(vd))
    vd match
      case Left(_) => assert(true)
      case _ => assert(false)


  test("VivaDuration fits into interval"):
    for {
      vd <- VivaDuration.from("01:00:00")
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T12:00:00")
    } yield assert(vd.fitsInto(start, end))


  test("VivaDuration does not fit into interval"):
    for {
      vd <- VivaDuration.from("03:00:00")
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T12:00:00")
    } yield assert(!vd.fitsInto(start, end))


  test("VivaDuration fits into interval exactly"):
    for {
      vd <- VivaDuration.from("02:00:00")
      start <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T10:00:00")
      end <- IntervalDateTime.from(InvalidTimeInterval.apply)("2022-01-02T12:00:00")
    } yield assert(vd.fitsInto(start, end))