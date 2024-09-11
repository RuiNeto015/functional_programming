package pj.properties

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties, Test}
import pj.domain.*
import pj.domain.SimpleTypes.*
import pj.domain.schedule.ScheduleMS01.execScheduleMS01
import pj.domain.schedule.ScheduleMS03.execScheduleMS03
import pj.domain.services.IntervalService
import pj.domain.services.IntervalService.*
import pj.properties.Generators.*

class ScheduleServiceProperties(algorithm: Agenda => Result[Schedule]) extends Properties("ScheduleService"):

  val MIN_SUCCESSFUL_TESTS = 50
  val NUMBER_OF_RESOURCES_TO_GENERATE = 4
  val NUMBER_OF_VIVA_TITLES_TO_GENERATE = 500
  val NUMBER_OF_VIVA_STUDENTS_TO_GENERATE = 500
  val MIN_OBLIGATE_VIVAS = 1
  val MAX_OBLIGATE_VIVAS = 3
  val MIN_NUM_VIVAS = 1
  val MAX_NUM_VIVAS = 4

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params.withMinSuccessfulTests(MIN_SUCCESSFUL_TESTS)

  //Resources ---------------------------------------------------------------------------------------------------------

  property("Generated ResourceId must not exist in the list of existing ResourceIds (Teacher)") =
    val rids: List[ResourceId] = (1 to 500).toList.flatMap(i => ResourceId.from(f"T$i%03d") match {
      case Right(id) => Some(id)
      case Left(_) => None
    })

    forAll(genResourceId(true, rids)) { id =>
      !rids.contains(id)
    }

  property("Generated ResourceId must not exist in the list of existing ResourceIds (External)") =
    val rids: List[ResourceId] = (1 to NUMBER_OF_RESOURCES_TO_GENERATE).toList.flatMap(i => ResourceId.from(f"E$i%03d") match {
      case Right(id) => Some(id)
      case Left(_) => None
    })

    forAll(genResourceId(true, rids)) { id =>
      !rids.contains(id)
    }

  property("ResourceName must be valid") =
    forAll(genResourceName) { rName =>
      rName.toString.length >= RESOURCE_NAME_LENGTH_MIN && rName.toString.length <= RESOURCE_NAME_LENGTH_MAX
    }

  property("External must have the obligate availabilities plus some extras (if case) and cannot overlap") =
    forAll({
      for {
        obligateIntervals <- Gen.chooseNum(MIN_OBLIGATE_VIVAS, MAX_OBLIGATE_VIVAS)
        interval <- Gen.listOfN(obligateIntervals, genGlobalIntervalTime)
        resource <- genExternal(interval, List())
      } yield (interval, resource)
    }) { args =>
      val obligateIntervals = args._1
      val external = args._2

      val obligateMatchSize = external.availabilities.filter(availability =>
        obligateIntervals.exists(i => IntervalService.equals(availability.timeInterval, i))
      ).sizeCompare(obligateIntervals)

      obligateMatchSize <= external.availabilities.length && allIntervalsDontOverlap(external.availabilities)
    }

  property("Teacher must have the obligate availabilities plus some extras (if case) and cannot overlap") =
    forAll({
      for {
        obligateIntervals <- Gen.chooseNum(MIN_OBLIGATE_VIVAS, MAX_OBLIGATE_VIVAS)
        interval <- Gen.listOfN(obligateIntervals, genGlobalIntervalTime)
        teacher <- genTeacher(interval, List())
      } yield (interval, teacher)
    }) { args =>
      val obligateIntervals = args._1
      val teacher = args._2

      val obligateMatchSize = teacher.availabilities.filter(availability =>
        obligateIntervals.exists(i => IntervalService.equals(availability.timeInterval, i))
      ).sizeCompare(obligateIntervals)

      obligateMatchSize <= teacher.availabilities.length && allIntervalsDontOverlap(teacher.availabilities)
    }

  // AvailabilityAgendaPreference --------------------------------------------------------------------------------------

  property("AvailabilityAgendaPreference must be valid") =
    forAll(getAvailabilityAgendaPreference) { aap =>
      aap.toString.toInt >= AVAILABILITY_AGENDA_PREFERENCE_MIN && aap.toString.toInt <= AVAILABILITY_AGENDA_PREFERENCE_MAX
    }

  // VivaTitle ---------------------------------------------------------------------------------------------------------

  property("Generated VivaTitle must not exist in the list of existing VivaTitles") =
    val vtl: List[VivaTitle] = (1 to NUMBER_OF_VIVA_TITLES_TO_GENERATE).toList.flatMap(i => VivaTitle.from(f"Viva Title $i") match {
      case Right(vt) => Some(vt)
      case Left(_) => None
    })

    forAll(genVivaTitle(vtl)) { title =>
      !vtl.contains(title)
    }

  // VivaStudent -------------------------------------------------------------------------------------------------------

  property("Generated VivaStudent must not exist in the list of existing VivaStudents") =
    val vsl: List[VivaStudent] = (1 to NUMBER_OF_VIVA_STUDENTS_TO_GENERATE).toList.flatMap(i => VivaStudent.from(f"Viva Student $i") match {
      case Right(vs) => Some(vs)
      case Left(_) => None
    })

    forAll(genVivaStudent(vsl)) { student =>
      !vsl.contains(student)
    }

  // VivaDuration ------------------------------------------------------------------------------------------------------

  property("VivaDuration with ceiling must be less than or equal to VivaDuration") =
    forAll(for {
      duration <- genDuration
      durationWithCeiling <- genDurationWithCeiling(duration)
    } yield (duration, durationWithCeiling)) { case (duration, durationWithCeiling) =>
      duration.toSeconds >= durationWithCeiling.toSeconds
    }

  // IntervalTime ------------------------------------------------------------------------------------------------------

  property("Availabilities are generated respecting the time intervals given") =
    forAll({
      for {
        intervalNum <- Gen.chooseNum(MIN_OBLIGATE_VIVAS, MAX_OBLIGATE_VIVAS)
        intervalList <- Gen.listOfN(intervalNum, genGlobalIntervalTime)
        availabilities <- genAvailabilities(intervalList)
      } yield (intervalList, availabilities)
    }) { args =>
      val intervalList = args._1
      val availabilities = args._2

      val intervalsFound = availabilities.foldLeft(0)((acc, av) =>
        if intervalList.exists(i => i.start == av.start && i.end == av.end) then acc + 1 else acc)

      intervalsFound == intervalList.length
    }

  property("Vivas time intervals are correctly generated respecting the limits of the time interval given") =
    forAll({
      for {
        globalDuration <- genDuration
        globalInterval <- genGlobalIntervalTime(globalDuration)
        vivaDuration <- genDurationWithCeiling(globalDuration)
        numIntervals <- Gen.chooseNum(MIN_NUM_VIVAS, MAX_NUM_VIVAS)
        intervalsOfVivas <- Gen.listOfN(numIntervals, genVivaIntervalTimeOf(globalInterval, vivaDuration))
      } yield (globalInterval, vivaDuration, numIntervals, intervalsOfVivas)
    }) { args =>
      val globalInterval = args._1
      val vivaDuration = args._2
      val numIntervals = args._3
      val intervalsOfVivas = args._4

      val isNumIntervalsCorrect = intervalsOfVivas.sizeIs.==(numIntervals)

      val isIntervalsCorrect = intervalsOfVivas.foldLeft(0)((acc, interval) =>
        if intervalHasExactDuration(interval, vivaDuration) &&
          (starts(interval, globalInterval) ||
            IntervalService.equals(interval, globalInterval) ||
            during(interval, globalInterval) ||
            finishes(interval, globalInterval)
            ) then acc + 1 else acc
      ) == numIntervals

      isNumIntervalsCorrect && isIntervalsCorrect
    }

  property("IntervalDateTime has the duration given") =
    forAll({
      for {
        duration <- genDuration
        interval <- genGlobalIntervalTime(duration)
      } yield (duration, interval)
    }) { args =>
      val duration = args._1
      val interval = args._2

      interval.end.toSeconds - interval.start.toSeconds == duration.toSeconds
    }

  // Resource Allocation for Interval ----------------------------------------------------------------------------------

  def genResourcesAllocation: Gen[((List[String], List[String]), Int, Int, List[String], String)] =
    for {
      resourceFlag <- Gen.pick(1, List("T", "E")).flatMap(seq => seq.headOption.fold(Gen.fail)(e => e))
      numOfAvailableResources <-
        if resourceFlag == "T"
        then Gen.chooseNum(AGENDA_TEACHERS_MIN, AGENDA_TEACHERS_MAX)
        else Gen.chooseNum(AGENDA_EXTERNALS_MIN, AGENDA_EXTERNALS_MAX)
      availableResources <- Gen.const(Generators.buildResources(resourceFlag, numOfAvailableResources))
      numOfEligibleResources <- Gen.chooseNum(0, availableResources.size)
      eligibleResources <- Gen.pick(numOfEligibleResources, availableResources)
      numOfRequiredResources <-
        if resourceFlag == "T"
        then Gen.chooseNum(2, TEACHERS_PER_VIVA_MAX)
        else Gen.chooseNum(EXTERNALS_PER_VIVA_MIN, EXTERNALS_PER_VIVA_MAX)
      allocation <- genResourceAllocationForInterval(resourceFlag, eligibleResources.toList, numOfRequiredResources,
        numOfAvailableResources + 1)
    } yield (
      allocation,
      numOfRequiredResources,
      numOfRequiredResources - numOfEligibleResources,
      availableResources,
      resourceFlag
    )

  property("Size of allocated resources must be equal to the num of required resources") =
    forAll(genResourcesAllocation):
      result => result._1._1.sizeIs == result._2


  property("Size of generated (extra) resources must be greater or equal than the difference between the required and eligible") =
    forAll(genResourcesAllocation):
      result => result._1._2.sizeIs >= result._3

  property("Generated (extra) resources indexes must be the subsequent to the available resources indexes") =
    forAll(genResourcesAllocation):
      result => {
        if result._1._2.isEmpty
        then true
        else result._4.find(r => r.matches(s"^${result._5}.*")).fold(true)(r => {
          try
            val maxIndex = r.substring(1).toInt
            result._1._2.count(r => r.substring(1).toInt > maxIndex) == result._1._2.size
          catch
            case e: Exception => false
        })
      }

  // Resource Allocation for Set of Intervals --------------------------------------------------------------------------

  property("Overlapping intervals cannot have coincident resources") =
    forAll({
      for {
        numOfVivas <- Gen.chooseNum(AGENDA_VIVAS_MIN, AGENDA_VIVAS_MAX)
        globalDuration <- genDuration
        globalTimeInterval <- genGlobalIntervalTime(globalDuration)
        vivaDuration <- genDurationWithCeiling(globalDuration)
        vivaIntervals <- Gen.listOfN(numOfVivas, genVivaIntervalTimeOf(globalTimeInterval, vivaDuration))
        numOfAgendaTeachers <- Gen.chooseNum(AGENDA_TEACHERS_MIN, AGENDA_TEACHERS_MAX)
        numOfAgendaExternals <- Gen.chooseNum(AGENDA_EXTERNALS_MIN, AGENDA_EXTERNALS_MAX)
        allocation <- {
          val resources = buildResources("T", numOfAgendaTeachers) ::: buildResources("E", numOfAgendaExternals)
          genResourceAllocationForSetOfIntervals(resources, vivaIntervals)
        }
      } yield allocation
    }):
      allocation => {
        allocation._2.forall(i => {
          allocation._2.filter(a => !(a eq i)).forall(innerI => {
            // intersects and has common resources
            if !(precedes(i._1, innerI._1) ||
              meets(i._1, innerI._1) ||
              metBy(i._1, innerI._1) ||
              precededBy(i._1, innerI._1)) &&
              (i._2 diff innerI._2).sizeIs != i._2.sizeIs
            then false
            else true
          })
        })
      }

  property("Non repetition of resources within a single interval") =
    forAll({
      for {
        numOfVivas <- Gen.chooseNum(AGENDA_VIVAS_MIN, AGENDA_VIVAS_MAX)
        globalDuration <- genDuration
        globalTimeInterval <- genGlobalIntervalTime(globalDuration)
        vivaDuration <- genDurationWithCeiling(globalDuration)
        vivaIntervals <- Gen.listOfN(numOfVivas, genVivaIntervalTimeOf(globalTimeInterval, vivaDuration))
        numOfAgendaTeachers <- Gen.chooseNum(AGENDA_TEACHERS_MIN, AGENDA_TEACHERS_MAX)
        numOfAgendaExternals <- Gen.chooseNum(AGENDA_EXTERNALS_MIN, AGENDA_EXTERNALS_MAX)
        allocation <- {
          val resources = buildResources("T", numOfAgendaTeachers) ::: buildResources("E", numOfAgendaExternals)
          genResourceAllocationForSetOfIntervals(resources, vivaIntervals)
        }
      } yield allocation
    }):
      allocation => {
        allocation._2.forall(i => {
          i._2.distinct.sizeIs == i._2.sizeIs
        })
      }

  // Agenda ------------------------------------------------------------------------------------------------------------

  /**
   * Verifies if one AvailabilityAgenda is scheduled to the AvailabilitySchedule provided
   */
  val isAgendaIntervalScheduled: (AvailabilitySchedule, AvailabilityAgenda) => Boolean =
    (scheduleInterval: AvailabilitySchedule, agendaInterval: AvailabilityAgenda) =>
      starts(scheduleInterval, agendaInterval) ||
        IntervalService.equals(scheduleInterval, agendaInterval) ||
        during(scheduleInterval, agendaInterval) ||
        finishes(scheduleInterval, agendaInterval)

  /**
   * Receives an AvailabilitySchedule and returns what is the preference of the corresponding resources' AvailabilityAgenda
   */
  def prefOfResourceScheduleInterval(scheduleInterval: AvailabilitySchedule,
                                     agendaIntervalList: List[AvailabilityAgenda]): Option[AvailabilityAgendaPreference] =
    agendaIntervalList.find(agendaInterval => isAgendaIntervalScheduled(scheduleInterval, agendaInterval)) match
      // Checks if it was found a compatible interval
      case Some(i) => Some(i.preference)
      case None => None

  /**
   * Returns the sum of all viva's resources' preference that matches the AvailabilitySchedule
   */
  def sumPrefVivaAgainstIntervalSchedule(scheduleInterval: AvailabilitySchedule,
                                         agendaViva: Viva): Option[SchedulePreference] =
    val resources = agendaViva.jury.president :: agendaViva.jury.advisor :: agendaViva.jury.coAdvisors ::: agendaViva.jury.supervisors
    resources.foldLeft(Option(SchedulePreference.zero))((accOpt, resource) =>
      accOpt.flatMap(acc =>
        prefOfResourceScheduleInterval(scheduleInterval, resource.availabilities).map(pref => acc + pref))
    )

  property("The number of scheduled vivas must be equals to the agenda's vivas") =
    forAll(genAgenda):
      agenda => {
        algorithm(agenda) match
          case Right(schedule) => schedule.scheduledVivas.sizeCompare(agenda.vivas) == 0
          case Left(e) => false
      }

  property("Schedule total preference must always be equals to the sum of all agenda viva's resource's preference") =
    // Calculates the sum of agenda's viva's preferences taking in consideration the intervals already scheduled for each one
    def sumPrefVivasList(scheduleVivaList: List[ScheduleViva], agendaVivaList: List[Viva]): Option[SchedulePreference] =
      scheduleVivaList.foldLeft(Option(SchedulePreference.zero))((accOpt, scheduleViva) =>
        agendaVivaList.find(agendaViva => agendaViva.title == scheduleViva.viva.title) match
          case Some(v) => accOpt.flatMap(acc =>
            sumPrefVivaAgainstIntervalSchedule(scheduleViva.aSchedule, v).map(pref => acc + pref))
          case _ => None
      )

    forAll(genAgenda):
      agenda => {
        algorithm(agenda) match
          case Right(schedule) => sumPrefVivasList(schedule.scheduledVivas, agenda.vivas) match
            case Some(calcPref) => calcPref == schedule.preference
            case _ => false
          case Left(e) => false
      }

  property("Schedule total preference must always be equals to the sum of each scheduled viva's preference") =
    forAll(genAgenda):
      agenda => {
        algorithm(agenda) match
          case Left(e) => false
          case Right(schedule) =>
            val calcPref = schedule.scheduledVivas.foldLeft(SchedulePreference.zero)((acc, scheduleViva) =>
              acc + scheduleViva.aSchedule.preference
            )
            schedule.preference == calcPref
      }

  property("Each schedule viva's info must always be equals to each corresponding agenda viva's info") =
    def sameResources(scheduleJury: VivaJury, agendaJury: VivaJury): Boolean =
      // Validates the coAdvisors
      val coAdvisorsAgendaMatches = agendaJury.coAdvisors.foldLeft(0)((acc, agendaCoAdvisor) =>
        scheduleJury.coAdvisors.find(scheduleResource => agendaCoAdvisor.rName == scheduleResource.rName &&
          agendaCoAdvisor.rId == scheduleResource.rId) match
          case Some(_) => acc + 1
          case _ => acc
      )

      // Validates the superAdvisors
      val superAdvisorsAgendaMatches = agendaJury.supervisors.foldLeft(0)((acc, agendaSuperAdvisor) =>
        scheduleJury.supervisors.find(scheduleResource => agendaSuperAdvisor.rName == scheduleResource.rName &&
          agendaSuperAdvisor.rId == scheduleResource.rId) match
          case Some(_) => acc + 1
          case _ => acc
      )

      coAdvisorsAgendaMatches == scheduleJury.coAdvisors.length &&
        superAdvisorsAgendaMatches == scheduleJury.supervisors.length &&
        scheduleJury.president.rName == agendaJury.president.rName && scheduleJury.president.rId == agendaJury.president.rId &&
        scheduleJury.advisor.rName == agendaJury.advisor.rName && scheduleJury.advisor.rId == agendaJury.advisor.rId

    def vivasInfoEquals(scheduleViva: ScheduleViva, agendaViva: Viva): Boolean =
      sumPrefVivaAgainstIntervalSchedule(scheduleViva.aSchedule, agendaViva) match
        case Some(calcPref) => scheduleViva.viva.title == agendaViva.title &&
          scheduleViva.viva.student == agendaViva.student &&
          scheduleViva.aSchedule.preference == calcPref &&
          sameResources(scheduleViva.viva.jury, agendaViva.jury)
        case _ => false

    forAll(genAgenda):
      agenda => {
        algorithm(agenda) match
          case Left(e) => false
          case Right(schedule) =>
            val numValidVivas = schedule.scheduledVivas.foldLeft(0)((acc, scheduleViva) =>
              agenda.vivas.find(agendaViva => agendaViva.title == scheduleViva.viva.title) match
                case Some(agendaViva) => if vivasInfoEquals(scheduleViva, agendaViva) then acc + 1 else acc
                case _ => acc
            )
            numValidVivas == schedule.scheduledVivas.length
      }

  property("Each viva's schedule duration is equal to the agenda's viva duration") =
    forAll(genAgenda):
      agenda => {
        algorithm(agenda) match
          case Left(e) => false
          case Right(schedule) =>
            schedule.scheduledVivas.forall(scheduleViva =>
              intervalHasExactDuration(scheduleViva.aSchedule.timeInterval, agenda.duration)
            )
      }

  property("The schedule's vivas resource cannot overlap with another schedule viva") =
    def getResources(scheduleViva: ScheduleViva): List[Resource] =
      scheduleViva.viva.jury.president :: scheduleViva.viva.jury.advisor ::
        scheduleViva.viva.jury.coAdvisors ::: scheduleViva.viva.jury.supervisors

    def findVivasThatIntersect(thisSchedule: ScheduleViva, scheduleVivaList: List[ScheduleViva]): List[ScheduleViva] =
      scheduleVivaList.foldLeft(List[ScheduleViva]())((acc, thatSchedule) =>
        if (intervalsOverlap(thisSchedule.aSchedule.timeInterval, thatSchedule.aSchedule.timeInterval))
          thatSchedule :: acc
        else
          acc
      )

    forAll(genAgenda):
      agenda =>
        algorithm(agenda) match
          case Left(e) => false
          case Right(schedule) =>
            schedule.scheduledVivas.forall { thisScheduleViva =>
              val thisResources = getResources(thisScheduleViva)
              val filteredScheduleVivas = schedule.scheduledVivas.filter(s =>
                s.viva.title != thisScheduleViva.viva.title)

              findVivasThatIntersect(thisScheduleViva, filteredScheduleVivas).forall { intersectSchedule =>
                val intersectResources = getResources(intersectSchedule)

                thisResources.forall { thisResource =>
                  !intersectResources.exists(thatResource => thatResource.rId == thisResource.rId)
                }
              }
            }

  property("Invalid agendas have one resource that is outside the intersection scope") =
    forAll(genInvalidAgenda) {
      agenda => {
        agenda.vivas.filter(v => {
          val resources = List(v.jury.president, v.jury.advisor) ::: v.jury.coAdvisors ::: v.jury.supervisors
          val resourcesWithOneAvailability = resources.filter(r => r.availabilities.sizeIs == 1)

          resourcesWithOneAvailability.filter(r => {
            resources.filter(innerR => !(innerR equals r)).filter(innerR => {
              allIntervalsDontOverlap(r.availabilities.map(a => a.timeInterval) ::: innerR.availabilities.map(a => a.timeInterval))
            }).sizeIs >= 1
          }).sizeIs >= 1
        }).sizeIs >= 1
      }
    }