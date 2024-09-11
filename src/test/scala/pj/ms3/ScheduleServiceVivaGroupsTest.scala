package pj.ms3

import org.scalatest.funsuite.AnyFunSuiteLike
import pj.domain.*
import pj.domain.DomainError.{InvalidPreference, InvalidTimeInterval}
import pj.domain.SimpleTypes.*
import pj.domain.schedule.ScheduleMS03.createSubGroupsOfVivas
import pj.domain.services.ScheduleService.vivasThatHasDirectDependencies

class ScheduleServiceVivaGroupsTest extends AnyFunSuiteLike:

  test("vivasThatHasDirectDependencies - with direct dependencies"):
    val availability = for {
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval <- TimeInterval.from(start, end)
      availability <- Right(AvailabilityAgenda(timeInterval, preference))
    } yield availability

    // Resources
    val t1 = for {
      id <- ResourceId.from("T001")
      name <- ResourceName.from("Teacher 001")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t2 = for {
      id <- ResourceId.from("T002")
      name <- ResourceName.from("Teacher 002")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t3 = for {
      id <- ResourceId.from("T003")
      name <- ResourceName.from("Teacher 003")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t4 = for {
      id <- ResourceId.from("T004")
      name <- ResourceName.from("Teacher 004")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t5 = for {
      id <- ResourceId.from("T005")
      name <- ResourceName.from("Teacher 005")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t6 = for {
      id <- ResourceId.from("T006")
      name <- ResourceName.from("Teacher 006")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val e1 = for {
      id <- ResourceId.from("E001")
      name <- ResourceName.from("External 001")
      availability: AvailabilityAgenda <- availability
      e <- External.from(id, name, List(availability))
    } yield e

    // Vivas
    val v1 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v1")
      student <- VivaStudent.from("student1")
    } yield Viva(title, student, jury)

    val v2 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      teacher3: Teacher <- t3
      external1: External <- e1
      jury <- VivaJury.from(president, advisor, List(external1, teacher3), List())

      title <- VivaTitle.from("v2")
      student <- VivaStudent.from("student2")
    } yield Viva(title, student, jury)

    val v3 = for {
      president: Teacher <- t3
      advisor: Teacher <- t4
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v3")
      student <- VivaStudent.from("student3")
    } yield Viva(title, student, jury)

    val v4 = for {
      president: Teacher <- t5
      advisor: Teacher <- t6
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v4")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val result = for {
      viva1 <- v1
      viva2 <- v2
      viva3 <- v3
      viva4 <- v4

      // V1
      res1 <- Right(vivasThatHasDirectDependencies(viva1, Set(viva1, viva2, viva3, viva4)))
      cond1 <- Right(res1.contains(viva2) && !res1.contains(viva3) && !res1.contains(viva4))
      // V2
      res2 <- Right(vivasThatHasDirectDependencies(viva2, Set(viva1, viva2, viva3, viva4)))
      cond2 <- Right(res2.contains(viva1) && res2.contains(viva3) && !res1.contains(viva4))
      // V3
      res3 <- Right(vivasThatHasDirectDependencies(viva3, Set(viva1, viva2, viva3, viva4)))
      cond3 <- Right(res3.contains(viva2) && !res3.contains(viva1) && !res1.contains(viva4))
      // V4
      res4 <- Right(vivasThatHasDirectDependencies(viva4, Set(viva1, viva2, viva3, viva4)))
      cond4 <- Right(!res4.contains(viva1) && !res4.contains(viva2) && !res4.contains(viva3))
    } yield cond1 && cond2 && cond3

    result match
      case Right(expr) => assert(expr)
      case Left(_) => assert(false)

  test("createSubGroupsOfVivas - with 2 dependent groups and 1 independent"):
    val availability = for {
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval <- TimeInterval.from(start, end)
      availability <- Right(AvailabilityAgenda(timeInterval, preference))
    } yield availability

    // Resources
    val t1 = for {
      id <- ResourceId.from("T001")
      name <- ResourceName.from("Teacher 001")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t2 = for {
      id <- ResourceId.from("T002")
      name <- ResourceName.from("Teacher 002")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t3 = for {
      id <- ResourceId.from("T003")
      name <- ResourceName.from("Teacher 003")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t4 = for {
      id <- ResourceId.from("T004")
      name <- ResourceName.from("Teacher 004")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t5 = for {
      id <- ResourceId.from("T005")
      name <- ResourceName.from("Teacher 005")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t6 = for {
      id <- ResourceId.from("T006")
      name <- ResourceName.from("Teacher 006")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t7 = for {
      id <- ResourceId.from("T007")
      name <- ResourceName.from("Teacher 007")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t8 = for {
      id <- ResourceId.from("T008")
      name <- ResourceName.from("Teacher 008")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t9 = for {
      id <- ResourceId.from("T009")
      name <- ResourceName.from("Teacher 009")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val e1 = for {
      id <- ResourceId.from("E001")
      name <- ResourceName.from("External 001")
      availability: AvailabilityAgenda <- availability
      e <- External.from(id, name, List(availability))
    } yield e

    val e2 = for {
      id <- ResourceId.from("E002")
      name <- ResourceName.from("External 002")
      availability: AvailabilityAgenda <- availability
      e <- External.from(id, name, List(availability))
    } yield e

    // Vivas
    val v1 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v1")
      student <- VivaStudent.from("student1")
    } yield Viva(title, student, jury)

    val v2 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      teacher3: Teacher <- t3
      external1: External <- e1
      jury <- VivaJury.from(president, advisor, List(external1, teacher3), List())

      title <- VivaTitle.from("v2")
      student <- VivaStudent.from("student2")
    } yield Viva(title, student, jury)

    val v3 = for {
      president: Teacher <- t3
      advisor: Teacher <- t4
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v3")
      student <- VivaStudent.from("student3")
    } yield Viva(title, student, jury)

    val v4 = for {
      president: Teacher <- t5
      advisor: Teacher <- t6
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v4")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val v5 = for {
      president: Teacher <- t7
      advisor: Teacher <- t8
      external2: External <- e2
      jury <- VivaJury.from(president, advisor, List(), List(external2))

      title <- VivaTitle.from("v5")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val v6 = for {
      president: Teacher <- t7
      advisor: Teacher <- t9
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v6")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val result = for {
      viva1: Viva <- v1
      viva2: Viva <- v2
      viva3: Viva <- v3
      viva4: Viva <- v4
      viva5: Viva <- v5
      viva6: Viva <- v6

      t1Id <- ResourceId.from("T001")
      t2Id <- ResourceId.from("T002")
      t3Id <- ResourceId.from("T003")
      t4Id <- ResourceId.from("T004")
      t5Id <- ResourceId.from("T005")
      t6Id <- ResourceId.from("T006")
      t7Id <- ResourceId.from("T007")
      t8Id <- ResourceId.from("T008")
      t9Id <- ResourceId.from("T009")
      e1Id <- ResourceId.from("E001")
      e2Id <- ResourceId.from("E002")

      (vivasDependencyGroup: List[(Set[Viva], Set[ResourceId])], vivasIndependent: Set[Viva])
        <- Right(createSubGroupsOfVivas(List(viva1, viva2, viva3, viva4, viva5, viva6)))

      cond1 = vivasIndependent.sizeIs.==(1) && vivasIndependent.contains(viva4)
      cond2 = vivasDependencyGroup.sizeIs.==(2) &&
        vivasDependencyGroup.contains(Set(viva1, viva2, viva3), Set(t1Id, t2Id, t3Id, t4Id, e1Id)) &&
        vivasDependencyGroup.contains(Set(viva5, viva6), Set(t7Id, t8Id, t9Id, e2Id))


    } yield cond1 && cond2

    result match
      case Right(expr) => assert(expr)
      case Left(_) => assert(false)

  test("createSubGroupsOfVivas - with direct dependencies"):
    val availability = for {
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("3")
      start: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-01T00:00:00")
      end: IntervalDateTime <- IntervalDateTime.from(InvalidTimeInterval.apply)("2024-01-02T00:00:00")
      timeInterval <- TimeInterval.from(start, end)
      availability <- Right(AvailabilityAgenda(timeInterval, preference))
    } yield availability

    // Resources
    val t1 = for {
      id <- ResourceId.from("T001")
      name <- ResourceName.from("Teacher 001")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t2 = for {
      id <- ResourceId.from("T002")
      name <- ResourceName.from("Teacher 002")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t3 = for {
      id <- ResourceId.from("T003")
      name <- ResourceName.from("Teacher 003")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t4 = for {
      id <- ResourceId.from("T004")
      name <- ResourceName.from("Teacher 004")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t5 = for {
      id <- ResourceId.from("T005")
      name <- ResourceName.from("Teacher 005")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t6 = for {
      id <- ResourceId.from("T006")
      name <- ResourceName.from("Teacher 006")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t7 = for {
      id <- ResourceId.from("T007")
      name <- ResourceName.from("Teacher 007")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t8 = for {
      id <- ResourceId.from("T008")
      name <- ResourceName.from("Teacher 008")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val t9 = for {
      id <- ResourceId.from("T009")
      name <- ResourceName.from("Teacher 009")
      availability: AvailabilityAgenda <- availability
      t <- Teacher.from(id, name, List(availability))
    } yield t

    val e1 = for {
      id <- ResourceId.from("E001")
      name <- ResourceName.from("External 001")
      availability: AvailabilityAgenda <- availability
      e <- External.from(id, name, List(availability))
    } yield e

    val e2 = for {
      id <- ResourceId.from("E002")
      name <- ResourceName.from("External 002")
      availability: AvailabilityAgenda <- availability
      e <- External.from(id, name, List(availability))
    } yield e

    // Vivas
    val v1 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v1")
      student <- VivaStudent.from("student1")
    } yield Viva(title, student, jury)

    val v2 = for {
      president: Teacher <- t1
      advisor: Teacher <- t2
      teacher3: Teacher <- t3
      external1: External <- e1
      jury <- VivaJury.from(president, advisor, List(external1, teacher3), List())

      title <- VivaTitle.from("v2")
      student <- VivaStudent.from("student2")
    } yield Viva(title, student, jury)

    val v3 = for {
      president: Teacher <- t3
      advisor: Teacher <- t4
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v3")
      student <- VivaStudent.from("student3")
    } yield Viva(title, student, jury)

    val v4 = for {
      president: Teacher <- t5
      advisor: Teacher <- t6
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v4")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val v5 = for {
      president: Teacher <- t1
      advisor: Teacher <- t3
      teacher7: Teacher <- t7
      external2: External <- e2
      jury <- VivaJury.from(president, advisor, List(teacher7), List(external2))

      title <- VivaTitle.from("v5")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val v6 = for {
      president: Teacher <- t7
      advisor: Teacher <- t9
      jury <- VivaJury.from(president, advisor, List(), List())

      title <- VivaTitle.from("v6")
      student <- VivaStudent.from("student")
    } yield Viva(title, student, jury)

    val result = for {
      viva1: Viva <- v1
      viva2: Viva <- v2
      viva3: Viva <- v3
      viva4: Viva <- v4
      viva5: Viva <- v5
      viva6: Viva <- v6

      (vivasDependencyGroup: List[(Set[Viva], Set[ResourceId])], vivasIndependent: Set[Viva])
        <- Right(createSubGroupsOfVivas(List(viva1, viva2, viva3, viva4, viva5, viva6)))

      t1Id <- ResourceId.from("T001")
      t2Id <- ResourceId.from("T002")
      t3Id <- ResourceId.from("T003")
      t4Id <- ResourceId.from("T004")
      t5Id <- ResourceId.from("T005")
      t6Id <- ResourceId.from("T006")
      t7Id <- ResourceId.from("T007")
      t8Id <- ResourceId.from("T008")
      t9Id <- ResourceId.from("T009")
      e1Id <- ResourceId.from("E001")
      e2Id <- ResourceId.from("E002")

      cond1 = vivasIndependent.sizeIs.==(1) && vivasIndependent.contains(viva4)
      cond2 = vivasDependencyGroup.sizeIs.==(1) &&
        vivasDependencyGroup.contains(Set(viva1, viva2, viva3, viva5, viva6), Set(t1Id, t2Id, t3Id, t4Id, t7Id, t9Id, e1Id, e2Id))
    } yield cond1 && cond2

    result match
      case Right(expr) => assert(expr)
      case Left(_) => assert(false)
