package pj.domain

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.{InvalidExternalAvailability, InvalidTimeInterval, XMLError}
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, ResourceId, ResourceName}
import pj.domain.{AvailabilityAgenda, External, Result, TimeInterval}

class ExternalResourceTest extends AnyFunSuite:

  val resourceIdR: Result[ResourceId] = ResourceId.from("T001")
  val resourceNameR: Result[ResourceName] = ResourceName.from("External")
  val preferenceR: Result[AvailabilityAgendaPreference] = 
    AvailabilityAgendaPreference.from(XMLError.apply)("3")

  test("external is valid 1 - close availabilities"):
    val result = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR

      // IntervalDateTime
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:31:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:31:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T16:30:00")

      // TimeIntervals
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      // AvailabilityAgenda
      aa1 <- Right(AvailabilityAgenda(ti1, preference))
      aa2 <- Right(AvailabilityAgenda(ti2, preference))
      aa3 <- Right(AvailabilityAgenda(ti3, preference))

      external <- External.from(resourceId, resourceName, List(aa1, aa2, aa3))
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(false)
      case Left(_) => assert(false)
      case Right(_: External) => assert(true)


  test("external is valid 2 - spaced availabilities"):
    val result = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR

      // IntervalDateTime
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T17:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T18:30:00")

      // TimeIntervals
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      // AvailabilityAgenda
      aa1 <- Right(AvailabilityAgenda(ti1, preference))
      aa2 <- Right(AvailabilityAgenda(ti2, preference))
      aa3 <- Right(AvailabilityAgenda(ti3, preference))

      external <- External.from(resourceId, resourceName, List(aa1, aa2, aa3))
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(false)
      case Left(_) => assert(false)
      case Right(_: External) => assert(true)

  test("external is valid 3 - unordered availabilities"):
    val result = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR

      // IntervalDateTime
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T19:30:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T21:30:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:30:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T13:30:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T15:30:00")

      // TimeIntervals
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      // AvailabilityAgenda
      aa1 <- Right(AvailabilityAgenda(ti1, preference))
      aa2 <- Right(AvailabilityAgenda(ti2, preference))
      aa3 <- Right(AvailabilityAgenda(ti3, preference))

      external <- External.from(resourceId, resourceName, List(aa1, aa2, aa3))
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(false)
      case Left(_) => assert(false)
      case Right(_: External) => assert(true)

  test("external is invalid 1 - intersection conflict in limits"):
    val result = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR

      // IntervalDateTime
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T09:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:59:59")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      // TimeIntervals
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      // AvailabilityAgenda
      aa1 <- Right(AvailabilityAgenda(ti1, preference))
      aa2 <- Right(AvailabilityAgenda(ti2, preference))
      aa3 <- Right(AvailabilityAgenda(ti3, preference))

      external <- External.from(resourceId, resourceName, List(aa1, aa2, aa3))
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(true)
      case Left(_) => assert(false)
      case Right(_: External) => assert(false)

  test("external is invalid 2 - intersection conflict inside of each"):
    val result = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR

      // IntervalDateTime
      idt1 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt2 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      idt3 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T08:00:00")
      idt4 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")

      idt5 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T10:00:00")
      idt6 <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-05-30T12:00:00")

      // TimeIntervals
      ti1 <- TimeInterval.from(idt1, idt2)
      ti2 <- TimeInterval.from(idt3, idt4)
      ti3 <- TimeInterval.from(idt5, idt6)

      // AvailabilityAgenda
      aa1 <- Right(AvailabilityAgenda(ti1, preference))
      aa2 <- Right(AvailabilityAgenda(ti2, preference))
      aa3 <- Right(AvailabilityAgenda(ti3, preference))

      external <- External.from(resourceId, resourceName, List(aa1, aa2, aa3))
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(true)
      case Left(_) => assert(false)
      case Right(_: External) => assert(false)

  test("external is invalid 3 - empty availabilities"):
    val result: Result[External] = for {
      resourceId <- resourceIdR
      resourceName <- resourceNameR
      preference <- preferenceR
      external <- External.from(resourceId, resourceName, List())
    } yield external

    result match
      case Left(_: InvalidExternalAvailability) => assert(true)
      case Left(_) => assert(false)
      case Right(external) => assert(false)