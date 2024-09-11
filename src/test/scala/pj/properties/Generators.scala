package pj.properties

import org.scalacheck.Gen
import pj.domain.*
import pj.domain.DomainError.*
import pj.domain.SimpleTypes.*
import pj.domain.services.IntervalService.*
import pj.domain.services.{IntervalService, ListService}

import java.time.{LocalDateTime, ZoneOffset}
import scala.annotation.tailrec

object Generators:

  val RESOURCE_ID_MIN = 1
  val RESOURCE_ID_MAX = 999
  val RESOURCE_NAME_LENGTH_MIN = 3
  val RESOURCE_NAME_LENGTH_MAX = 15
  val AVAILABILITY_AGENDA_PREFERENCE_MIN = 1
  val AVAILABILITY_AGENDA_PREFERENCE_MAX = 5
  val VIVA_TITLE_NUM_MIN = 1
  val VIVA_TITLE_NUM_MAX = 999
  val VIVA_STUDENT_NUM_MIN = 1
  val VIVA_STUDENT_NUM_MAX = 999
  val AGENDA_VIVAS_MAX = 5
  val AGENDA_VIVAS_MIN = 1
  val TEACHERS_PER_VIVA_MAX = 3
  val EXTERNALS_PER_VIVA_MIN = 1
  val EXTERNALS_PER_VIVA_MAX = 3
  val AGENDA_TEACHERS_MIN = 2
  val AGENDA_TEACHERS_MAX = 5
  val AGENDA_EXTERNALS_MIN = 1
  val AGENDA_EXTERNALS_MAX = 4
  val EXTRA_AVAILABILITIES_MIN = 1
  val EXTRA_AVAILABILITIES_MAX = 4
  val DATETIME_LIMIT_MIX: Long = LocalDateTime.of(2024, 1, 1, 0, 0).toEpochSecond(ZoneOffset.UTC)
  val DATETIME_LIMIT_MAX: Long = LocalDateTime.of(2024, 2, 1, 0, 0).toEpochSecond(ZoneOffset.UTC)

  /**
   * Converts a Result[T] into a Gen[T]
   *
   * @param result the Result to convert
   */
  private def transformResultIntoGen[T](result: Result[T]): Gen[T] =
    result.fold(e => Gen.fail, r => Gen.const(r))

  /**
   * Generates a ResourceId
   *
   * @param isTeacher defines if the resource to generate is a teacher or an external
   * @param rids      the list of existing ResourceIds
   */
  def genResourceId(isTeacher: Boolean, rids: List[ResourceId]): Gen[ResourceId] =
    val prefix = if (isTeacher) "T" else "E"

    val allPossibleIds = (RESOURCE_ID_MIN to RESOURCE_ID_MAX).map(num => ResourceId.from(prefix + "%03d".format(num)))
      .collect { case Right(resourceId) => resourceId }

    val remainingIds = allPossibleIds.diff(rids)

    Gen.oneOf(remainingIds)

  /**
   * Generates a ResourceName
   *
   */
  def genResourceName: Gen[ResourceName] =
    for {
      charsLen <- Gen.chooseNum(RESOURCE_NAME_LENGTH_MIN, RESOURCE_NAME_LENGTH_MAX)
      chars <- Gen.listOfN(charsLen, Gen.alphaLowerChar)
      rn <- transformResultIntoGen(ResourceName.from(chars.mkString))
    } yield rn

  /**
   * Generates an AvailabilityAgendaPreference
   *
   */
  def getAvailabilityAgendaPreference: Gen[AvailabilityAgendaPreference] =
    for {
      p <- Gen.chooseNum(AVAILABILITY_AGENDA_PREFERENCE_MIN, AVAILABILITY_AGENDA_PREFERENCE_MAX)
      aap <- transformResultIntoGen(AvailabilityAgendaPreference.from(DomainError.InvalidPreference.apply)(p.toString))
    } yield aap

  /**
   * Generates a VivaTitle
   *
   */
  def genVivaTitle(vtl: List[VivaTitle]): Gen[VivaTitle] =
    val allPossibleTitles = (VIVA_TITLE_NUM_MIN to VIVA_TITLE_NUM_MAX).map(num =>
        VivaTitle.from("Viva Title " + "%d".format(num)))
      .collect { case Right(title) => title }

    val remainingTitles = allPossibleTitles.diff(vtl)

    Gen.oneOf(remainingTitles)

  /**
   * Generates a VivaStudent
   *
   */
  def genVivaStudent(vsl: List[VivaStudent]): Gen[VivaStudent] =
    val allPossibleStudents = (VIVA_STUDENT_NUM_MIN to VIVA_STUDENT_NUM_MAX).map(num =>
        VivaStudent.from("Viva Student " + "%d".format(num)))
      .collect { case Right(student) => student }

    val remainingStudents = allPossibleStudents.diff(vsl)

    Gen.oneOf(remainingStudents)

  /**
   * Generates a VivaDuration
   *
   */
  def genDuration: Gen[VivaDuration] =
    for {
      h <- Gen.choose(0, 23)
      m <- Gen.choose(0, 59)
      s <- if (h == 0 && m == 0) Gen.choose(1, 59) else Gen.choose(0, 59)
      vd <- transformResultIntoGen(VivaDuration.from(f"$h%02d:$m%02d:$s%02d"))
    } yield vd

  /**
   * Generates a VivaDuration with ceiling
   *
   * @param ceiling the ceiling VivaDuration
   */
  def genDurationWithCeiling(ceiling: VivaDuration): Gen[VivaDuration] =
    for {
      possibleDurationInSeconds <- Gen.chooseNum(1, ceiling.toSeconds)
      h <- Gen.chooseNum(0, possibleDurationInSeconds / 3600)
      remainingSecondsAfterHours = possibleDurationInSeconds - (h * 3600)
      m <- Gen.chooseNum(0, remainingSecondsAfterHours / 60)
      remainingSecondsAfterMinutes = remainingSecondsAfterHours - (m * 60)
      s <- Gen.chooseNum(0, remainingSecondsAfterMinutes % 60)
      vd <- transformResultIntoGen(VivaDuration.from(f"$h%02d:$m%02d:$s%02d"))
    } yield vd

  /**
   * Generates a TimeInterval of a random valid start date that has the duration passed
   *
   * @param duration the duration of the interval
   */
  def genGlobalIntervalTime(duration: VivaDuration): Gen[TimeInterval] =
    for {
      start <- Gen.choose(DATETIME_LIMIT_MIX, DATETIME_LIMIT_MIX)

      idtStart <- transformResultIntoGen(IntervalDateTime.from(InvalidTimeInterval.apply)(start))
      idtEnd <- transformResultIntoGen(IntervalDateTime.from(InvalidTimeInterval.apply)(duration.toSeconds + start))

      interval <- transformResultIntoGen(TimeInterval.from(idtStart, idtEnd))
    } yield interval

  /**
   * Generates a TimeInterval of a random valid start date that has a random duration
   *
   */
  def genGlobalIntervalTime: Gen[TimeInterval] =
    for {
      duration <- genDuration
      interval <- genGlobalIntervalTime(duration)
    } yield interval

  /**
   * Given a period of time and a duration, generates a random period of time within it
   *
   * @param globalTimeInterval the global interval
   * @param vivaDuration       the duration of the viva
   */
  def genVivaIntervalTimeOf(globalTimeInterval: TimeInterval, vivaDuration: VivaDuration): Gen[TimeInterval] =
    for {
      start <- Gen.chooseNum(globalTimeInterval.start.toSeconds, globalTimeInterval.end.toSeconds - vivaDuration.toSeconds)

      idtStart <- transformResultIntoGen(IntervalDateTime.from(InvalidTimeInterval.apply)(start))
      idtEnd <- transformResultIntoGen(IntervalDateTime.from(InvalidTimeInterval.apply)(start + vivaDuration.toSeconds))

      interval <- transformResultIntoGen(TimeInterval.from(idtStart, idtEnd))
    } yield interval

  /**
   * Given a list of TimeIntervals generates a list of AvailabilityAgenda
   *
   * @param intervalList time intervals to consider
   */
  def genAvailabilities(intervalList: List[TimeInterval]): Gen[List[AvailabilityAgenda]] =
    for {
      pref <- Gen.listOfN(intervalList.length, getAvailabilityAgendaPreference)
      res <- pref.zip(intervalList).map(r => AvailabilityAgenda(r._2, r._1))
    } yield res

  /**
   * Generates time intervals that dont overlap with a given list of already associated time intervals
   */
  def genIntervalsThatDontOverlap(intervalList: List[TimeInterval]): Gen[List[TimeInterval]] =
    val interval = for {
      extraNum <- Gen.chooseNum(EXTRA_AVAILABILITIES_MIN, EXTRA_AVAILABILITIES_MAX)
      i <- Gen.listOfN(extraNum, genGlobalIntervalTime)
    } yield i

    interval.flatMap { newIntervals =>
      newIntervals.foldLeft(List[TimeInterval]())((acc, newInterval) =>
        val validInterval = (newIntervals ::: intervalList).forall(currentInterval =>
          !intervalsOverlap(currentInterval, newInterval)
        )

        if validInterval then newInterval +: acc else acc
      )
    }

  /**
   * Given a TimeInterval list returns a list of TimeInterval with larger randomly intervals
   */
  def genRandomlyLargerIntervals(intervalList: List[TimeInterval]): Gen[List[TimeInterval]] =
    def getIntervalRandomlyLarger(thisTime: TimeInterval, list: List[TimeInterval]): TimeInterval =

      def genLocalDateTimeBetween(start: IntervalDateTime, end: IntervalDateTime): IntervalDateTime =
        val startEpochSecond = start.to.toEpochSecond(ZoneOffset.UTC)
        val endEpochSecond = end.to.toEpochSecond(ZoneOffset.UTC)

        val gen = for
          random <- Gen.choose(startEpochSecond, endEpochSecond)
        yield IntervalDateTime.from(LocalDateTime.ofEpochSecond(random, 0, ZoneOffset.UTC))

        gen.sample match
          case Some(x) => x
          case _ => start

      val minimums = list.foldLeft((Option.empty[IntervalDateTime], Option.empty[IntervalDateTime])):
        case ((accMinLeft, accMinRight), thatInterval) =>
          val newMinLeft = if (thatInterval.end.isBefore(thisTime.start))
            accMinLeft.map(minLeft => if (minLeft.isAfter(thatInterval.end)) minLeft else thatInterval.end).orElse(Some(thatInterval.end))
          else
            accMinLeft

          val newMinRight = if (thatInterval.start.isAfter(thisTime.end))
            accMinRight.map(minRight => if (minRight.isBefore(thatInterval.start)) minRight else thatInterval.start).orElse(Some(thatInterval.start))
          else
            accMinRight

          (newMinLeft, newMinRight)

      minimums match
        case (None, None) => thisTime
        case (Some(minLeft), Some(minRight)) =>
          val newStart = genLocalDateTimeBetween(minLeft, thisTime.start)
          val newEnd = genLocalDateTimeBetween(thisTime.end, minRight)
          TimeInterval.from(newStart, newEnd).fold(e => thisTime, newTime => newTime)
        case (None, Some(minRight)) =>
          val newEnd = genLocalDateTimeBetween(thisTime.end, minRight)
          TimeInterval.from(thisTime.start, newEnd).fold(e => thisTime, newTime => newTime)
        case (Some(minLeft), _) =>
          val newStart = genLocalDateTimeBetween(minLeft, thisTime.start)
          TimeInterval.from(newStart, thisTime.end).fold(e => thisTime, newTime => newTime)

    Gen.const(intervalList.foldLeft(List.from[TimeInterval](intervalList)) { (acc, interval) =>
      val newInterval = getIntervalRandomlyLarger(interval, acc.filterNot(i => IntervalService.equals(i, interval)))
      newInterval :: acc.filterNot(i => IntervalService.equals(i, interval))
    })


  /**
   * Given a list of TimeInterval, generates a External containing that intervals as availabilities and more
   *
   * @param intervalList intervals to convert into availabilities
   * @param resources    the list of Resources already generated
   */
  def genExternal(intervalList: List[TimeInterval], resources: List[Resource]): Gen[External] =

    val rids = resources.map(r => r.rId)
    for {
      id <- genResourceId(false, rids)
      name <- genResourceName
      randomlyLargerIntervals <- genRandomlyLargerIntervals(intervalList)
      availabilities <- genAvailabilities(randomlyLargerIntervals)
      extraNum <- Gen.chooseNum(EXTRA_AVAILABILITIES_MIN, EXTRA_AVAILABILITIES_MAX)
      extraIntervals <- genIntervalsThatDontOverlap(randomlyLargerIntervals)
      extraAvailabilities <- genAvailabilities(extraIntervals)

      external <- transformResultIntoGen(External.from(id, name, availabilities ::: extraAvailabilities))
    } yield external

  /**
   * Given a list of TimeInterval, generates a Teacher containing that intervals as availabilities
   *
   * @param intervalList intervals to convert into availabilities
   * @param resources    the list of Resources already generated
   */
  def genTeacher(intervalList: List[TimeInterval], resources: List[Resource]): Gen[Teacher] =

    val rids = resources.map(r => r.rId)
    for {
      id <- genResourceId(true, rids)
      name <- genResourceName
      randomlyLargerIntervals <- genRandomlyLargerIntervals(intervalList)
      availabilities <- genAvailabilities(randomlyLargerIntervals)
      extraNum <- Gen.chooseNum(EXTRA_AVAILABILITIES_MIN, EXTRA_AVAILABILITIES_MAX)
      extraIntervals <- genIntervalsThatDontOverlap(randomlyLargerIntervals)
      extraAvailabilities <- genAvailabilities(extraIntervals)

      teacher <- transformResultIntoGen(Teacher.from(id, name, availabilities ::: extraAvailabilities))
    } yield teacher

  /**
   * Finds the eligible resources for a predefined viva interval. This function iterates over the already processed
   * intervals (acc3) and checks if the targetInterval, which is the current interval being processed, intersects
   * with any of the processed intervals. If an intersection is found, it implies that the resources present in that
   * interval are not eligible, as one resource cannot be in two places at the same time.
   *
   * @param targetInterval     the predefined viva interval
   * @param availableResources the list of available resources
   * @param processedIntervals accumulator from the parent function that refers to the intervals that were already processed
   * @return list of the eligible resources
   */
  def findEligibleResources(targetInterval: TimeInterval, availableResources: List[String],
                            processedIntervals: List[(TimeInterval, List[String])]): List[String] =

    val result = processedIntervals.foldLeft(List[String](), availableResources)((acc, e) => {
      if (precedes(targetInterval, e._1) || meets(targetInterval, e._1) || metBy(targetInterval, e._1) || precededBy(targetInterval, e._1))
      // if they dont intersect then resources are eligible
        ((acc._1 ::: e._2.filter(a => acc._2.contains(a))).distinct, acc._2 diff e._2)
      else
        (acc._1 diff e._2, acc._2 diff e._2)
    })

    (result._1 ::: result._2).distinct

  /**
   * Generates a resource allocation for a predefined viva interval.
   *
   * @param resourceFlag      indicates the type of resource to allocate 'T' for teacher or 'E' for external
   * @param eligibleResources the list of eligible resources to allocate
   * @param requiredAmount    the amount of resources needed for allocation
   * @param currentIndex      the current index of the resource type
   * @return (list of allocated resources, list of generated resources) wrapped in a Gen
   */
  def genResourceAllocationForInterval(resourceFlag: String, eligibleResources: List[String], requiredAmount: Int,
                                       currentIndex: Int): Gen[(List[String], List[String])] =

    val amountToGen = requiredAmount - eligibleResources.size

    @tailrec
    def genExtraResources(generated: List[String], amountToGen: Int, index: Int): List[String] =
      if amountToGen <= 0
      then generated
      else genExtraResources(s"$resourceFlag$index" +: generated, amountToGen - 1, index + 1)

    val extraResources = genExtraResources(List(), amountToGen, currentIndex)
    Gen.pick(requiredAmount, eligibleResources ::: extraResources).flatMap(e => Gen.const((e.toList, extraResources)))

  /**
   * Generates a valid resource allocation for each predefined viva interval.
   *
   * @param resources     the list of available resources (the execution of this function can add new resources if
   *                      needed with the purpose of making the allocation possible and valid)
   * @param vivaIntervals the list of predefined viva intervals
   * @return (list of available resources, resources per each predefined viva interval) wrapped in a Gen
   */
  def genResourceAllocationForSetOfIntervals(resources: List[String], vivaIntervals: List[TimeInterval]): Gen[(List[String], List[(TimeInterval, List[String])])] =
    /*
      The following fold behaves like this:
      acc._1 is 'List[String]' -> refers to the list of all available resources
      acc._2 is 'List[(TimeInterval, List[String])' -> refers to the output of the function where each time interval has
      its respective resources allocated
     */
    val r = vivaIntervals.foldLeft(Gen.const((List[String](), List[(TimeInterval, List[String])]())))((acc, e) => {
      acc.flatMap(innerAcc => {
        val eligibleResources = findEligibleResources(e, resources ::: innerAcc._1, innerAcc._2)
        val eligibleTeachers = eligibleResources.filter(r => r.matches("^T.*"))
        val eligibleExternals = eligibleResources.filter(r => r.matches("^E.*"))

        val numOfAvailableTeachers = (innerAcc._1 ::: resources).count(r => r.matches("^T.*"))
        val numOfAvailableExternals = (innerAcc._1 ::: resources).count(r => r.matches("^E.*"))

        for {
          numOfTeachersToAllocate <- Gen.chooseNum(2, TEACHERS_PER_VIVA_MAX)
          teacherAllocation <- genResourceAllocationForInterval("T", eligibleTeachers, numOfTeachersToAllocate, numOfAvailableTeachers + 1)
          numOfExternalsToAllocate <- Gen.chooseNum(EXTERNALS_PER_VIVA_MIN, EXTERNALS_PER_VIVA_MAX)
          externalAllocation <- genResourceAllocationForInterval("E", eligibleExternals, numOfExternalsToAllocate, numOfAvailableExternals + 1)
        } yield {
          (innerAcc._1 ::: teacherAllocation._2 ::: externalAllocation._2,
            (e, teacherAllocation._1 ::: externalAllocation._1) +: innerAcc._2)
        }
      })
    })

    r.flatMap(out => (out._1 ::: resources, out._2))


  /**
   * Generates a viva jury given a list of resources. This method distributes the allocated resources randomly by the
   * viva jury roles, guaranteeing that the jury will be valid.
   *
   * @param resources the list of resources that will be part of the jury
   * @return a viva jury
   */
  def genVivaJury(resources: List[Resource]): Gen[VivaJury] =
    val teachers = resources.collect { case t: Teacher => t }
    val externals = resources.collect { case e: External => e }

    val jury = for {
      president <- Gen.pick(1, teachers).flatMap(seq => seq.headOption.fold(Gen.fail)(Gen.const))
      advisor <- Gen.pick(1, teachers diff List(president)).flatMap(seq => seq.headOption.fold(Gen.fail)(Gen.const))
      numOfCoAdvisorTeachers <- Gen.chooseNum(0, teachers.size - 2)
      numOfCoAdvisorExternals <- Gen.chooseNum(0, externals.size)
      tCoAdvisors <- Gen.pick(numOfCoAdvisorTeachers, teachers diff List(president, advisor))
      eCoAdvisors <- Gen.pick(numOfCoAdvisorExternals, externals)
      numOfSupervisors <- Gen.chooseNum(0, externals.size - numOfCoAdvisorExternals)
      supervisors <- Gen.pick(numOfSupervisors, externals diff eCoAdvisors)
    } yield VivaJury.from(president, advisor, tCoAdvisors.toList ::: eCoAdvisors.toList, supervisors.toList)

    jury.flatMap(jury => jury.fold(_ => Gen.fail, jury => jury))

  /**
   * Builds a list of raw resources.
   *
   * @param resourceFlag the resource flag indicating if it's teacher or external
   * @param counter      the number of resources to generate
   * @return the list of raw resources
   */
  def buildResources(resourceFlag: String, counter: Int): List[String] =
    @tailrec
    def loop(resources: List[String], counter: Int): List[String] =
      if counter <= 0
      then resources
      else loop(s"$resourceFlag$counter" +: resources, counter - 1)

    loop(List(), counter)

  /**
   * Generates a list of valid vivas.
   *
   * @param vivaIntervals        the set of predefined intervals for which the vivas will occur
   * @param numOfAgendaTeachers  the number of available teachers (this value may not correlate to the output if the
   *                             number of teachers is not enough to make the agenda valid)
   * @param numOfAgendaExternals the number of available externals (this value may not correlate to the output if the
   *                             number of externals is not enough to make the agenda valid)
   * @return a list of vivas
   */
  def genVivas(vivaIntervals: List[TimeInterval], numOfAgendaTeachers: Int, numOfAgendaExternals: Int): Gen[List[Viva]] =
    val resources = buildResources("T", numOfAgendaTeachers) ::: buildResources("E", numOfAgendaExternals)

    // Filters the intervals keeping only the ones for which the resource was allocated.
    def filterIntervals(resource: String, allocation: List[(TimeInterval, List[String])]): List[TimeInterval] =
      allocation.foldLeft(List[TimeInterval]())((acc, e) => {
        if e._2.contains(resource)
        then e._1 +: acc
        else acc
      })

    for {
      rawResourcesAllocation <- genResourceAllocationForSetOfIntervals(resources, vivaIntervals)

      // for each string resource creates the respective resource object
      resources <- rawResourcesAllocation._1.foldLeft(Gen.const(Map[String, Teacher | External]()))((acc, e) => {
        if e.matches("^T.*")
        then acc.flatMap(innerAcc => genTeacher(filterIntervals(e, rawResourcesAllocation._2), innerAcc.values.toList).flatMap(r => innerAcc + (e -> r)))
        else acc.flatMap(innerAcc => genExternal(filterIntervals(e, rawResourcesAllocation._2), innerAcc.values.toList).flatMap(r => innerAcc + (e -> r)))
      })
      resourcesAllocation <- rawResourcesAllocation._2.foldLeft(Gen.const(List[(TimeInterval, List[Resource])]()))((acc, e) => {
        acc.flatMap(innerAcc => {
          (e._1, e._2.foldLeft(List[Resource]())((acc, e) => resources.get(e).fold(acc)(r => r +: acc))) +: innerAcc
        })
      })
      vivas <- resourcesAllocation.foldLeft(Gen.const(List[Viva]()))((acc, e) => {
        acc.flatMap(innerAcc => {
          for {
            title <- genVivaTitle(innerAcc.map(v => v.title))
            student <- genVivaStudent(innerAcc.map(v => v.student))
            jury <- genVivaJury(e._2)
          } yield Viva(title, student, jury) +: innerAcc
        })
      })
    } yield vivas

  /**
   * Generates a valid agenda
   *
   * @return the agenda
   */
  def genAgenda: Gen[Agenda] =
    for {
      numOfVivas <- Gen.chooseNum(AGENDA_VIVAS_MIN, AGENDA_VIVAS_MAX)
      globalDuration <- genDuration
      globalTimeInterval <- genGlobalIntervalTime(globalDuration)
      vivaDuration <- genDurationWithCeiling(globalDuration)
      vivaIntervals <- Gen.listOfN(numOfVivas, genVivaIntervalTimeOf(globalTimeInterval, vivaDuration))
      numOfAgendaTeachers <- Gen.chooseNum(AGENDA_TEACHERS_MIN, AGENDA_TEACHERS_MAX)
      numOfAgendaExternals <- Gen.chooseNum(AGENDA_EXTERNALS_MIN, AGENDA_EXTERNALS_MAX)
      vivas <- genVivas(vivaIntervals, numOfAgendaTeachers, numOfAgendaExternals)
      agenda <- transformResultIntoGen(Agenda.from(vivaDuration, vivas))
    } yield agenda

  /**
   * Generates an invalid agenda
   *
   * @return the agenda
   */
  def genInvalidAgenda: Gen[Agenda] =
    for {
      numOfVivas <- Gen.chooseNum(AGENDA_VIVAS_MIN, AGENDA_VIVAS_MAX)
      globalDuration <- genDuration
      globalTimeInterval <- genGlobalIntervalTime(globalDuration)
      vivaDuration <- genDurationWithCeiling(globalDuration)
      vivaIntervals <- Gen.listOfN(numOfVivas, genVivaIntervalTimeOf(globalTimeInterval, vivaDuration))
      numOfAgendaTeachers <- Gen.chooseNum(AGENDA_TEACHERS_MIN, AGENDA_TEACHERS_MAX)
      numOfAgendaExternals <- Gen.chooseNum(AGENDA_EXTERNALS_MIN, AGENDA_EXTERNALS_MAX)
      vivas <- genVivas(vivaIntervals, numOfAgendaTeachers, numOfAgendaExternals)
      mVivas <- makeAgendaInvalid(vivaIntervals, vivas, globalTimeInterval)
      agenda <- transformResultIntoGen(Agenda.from(vivaDuration, mVivas))
    } yield agenda

  /**
   * Turns valid agenda into a invalid one
   *
   * @param intervals          the predefined intervals for the agenda vivas
   * @param vivas              the vivas of the agenda
   * @param globalTimeInterval the global time interval for which all intervals are constrained
   * @return the list of modified vivas that makes the agenda invalid
   */
  def makeAgendaInvalid(intervals: List[TimeInterval], vivas: List[Viva], globalTimeInterval: TimeInterval): Gen[List[Viva]] =
    Gen.pick(1, vivas).flatMap(viva => {
      val newViva = viva.map(v => {
        val resourceToModify = v.jury.president

        for {
          start <- IntervalDateTime.from(InvalidTimeInterval.apply)(globalTimeInterval.start.toSeconds - 1)
          end <- IntervalDateTime.from(InvalidTimeInterval.apply)(globalTimeInterval.start.toSeconds)
          interval <- TimeInterval.from(start, end)
          preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)("2")
          newResource <- Teacher.from(resourceToModify.rId, resourceToModify.rName, List(AvailabilityAgenda(interval, preference)))
          newVivaJury <- VivaJury.from(newResource, v.jury.advisor, v.jury.coAdvisors, v.jury.supervisors)
        } yield Viva(v.title, v.student, newVivaJury)
      })
      ListService.sequence(newViva.toList).fold(_ => Gen.fail, nv => nv ::: vivas diff viva)
    })