package pj.domain.schedule

import pj.domain.SimpleTypes.*
import pj.domain.services.IntervalService
import pj.domain.services.IntervalService.{intervalsOverlap, isInnerInterval}
import pj.domain.services.ListService.{sequence, sortList}
import pj.domain.services.ResourcesService.{vivaJuryToResourceIdList, vivaJuryToResourceList}
import pj.domain.services.ScheduleService.*
import pj.domain.services.ScheduleService.{addVivasToGroups, mergeGroups, parseAvailabilityAgenda, vivasThatHasDirectDependencies}
import pj.domain.xml.{DomainToXML, XMLToDomain}
import pj.domain.{Agenda, AvailabilityAgenda, AvailabilitySchedule, DomainError, Result, ScheduleViva, TimeInterval, Viva, Schedule as ScheduleDomain}

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.xml.Elem

object ScheduleMS03 extends Schedule:

  /**
   * Algorithm entry point of MS3
   *
   * @param xml the xml root element
   * @return the result xml element of the algorithm
   */
  def create(xml: Elem): Result[Elem] =
    val writer = DomainToXML.scheduleToXml("../../schedule.xsd")
    val agenda = XMLToDomain.parseToDomain(xml)

    agenda match
      case Right(a) => execScheduleMS03(a) match
        case Right(sd) => Right(writer(sd))
        case Left(e) => Left(e)
      case Left(e) => Left(e)

  /**
   * Executes the MS03 algorithm given a agenda
   *
   * @param agenda agenda data to process
   * @return a Result with a domain error or the Schedule
   */
  def execScheduleMS03(agenda: Agenda): Result[ScheduleDomain] =
    // obtains all the tuples that tracks the possible availabilities to each viva -> (Viva, List[Availability])
    transformAgendaIntoVivasWithPossibleAvailabilities(agenda) match
      case Left(e) => Left(e)
      case Right(vivasAvailabilities) =>
        // obtains the list of vivas that are independent, and the list of dependent groups
        val (dependentGroupVivas, independentVivas) = createSubGroupsOfVivas(agenda.vivas)

        // calculates the optimized schedule for the independent vivas
        val independentOptimizedScheduleVivas = optimizeIndependentVivas(vivasAvailabilities.filter(v => independentVivas.contains(v._1)))

        // obtains the optimized schedule for all the dependent groups of vivas
        val dependentOptimizedScheduleVivas = dependentGroupVivas.foldLeft(List[ScheduleViva]()) { (scheduleVivasAcc, dependentGroup) =>
          // transforms the group (Set[Viva], Set[ResourceId]) into List[(Viva, List[TimeInterval])]
          val transformedGroup = vivasAvailabilities
            .filter(v => dependentGroup._1.contains(v._1))
            .map(v => (v._1, v._2.map(av => av.timeInterval)))

          val disputedIntervals = findDisputedIntervals(transformedGroup)

          val intraVivaCombinations = intraVivaCombine(disputedIntervals, agenda.duration)

          val interVivaCombinations = interVivaCombine(intraVivaCombinations, agenda.duration)

          val optimizedScheduledCombination = getBestAllocation(vivasAvailabilities, interVivaCombinations)

          scheduleVivasAcc ::: optimizedScheduledCombination
        }

        val totalOptimizedScheduleVivas = independentOptimizedScheduleVivas ::: dependentOptimizedScheduleVivas
        val totalPreference: SchedulePreference = totalOptimizedScheduleVivas.map(s => s.aSchedule.preference)
          .foldLeft(SchedulePreference.zero) { (acc, value) => acc + value }
        ScheduleDomain.from(totalOptimizedScheduleVivas, totalPreference)

  /**
   * Given a list of vivas returns all the vivas divided by dependency/independent groups.
   * One dependent group represents a list of vivas that has dependency, e.g., V1 has shared resources with V2, and by it's turn
   * has dependency with V3, so [V1, V2, V3] is one group.
   *
   * @param vivas the vivas
   * @return list where each entry represents a group of vivas that has resource dependencies between them. Each entry
   *         inside of one group has the viva and the time intervals where it can occur.
   *
   */
  def createSubGroupsOfVivas(vivas: List[Viva]): (List[(Set[Viva], Set[ResourceId])], Set[Viva]) =
    val (vivasDependencyGroup, _, vivasIndependent) = vivas.foldLeft(
      (
        List.empty[(Set[Viva], Set[ResourceId])],
        Set.empty[VivaTitle],
        Set.empty[Viva]
      )
    ) { (acc, viva) =>
      val (groupAcc, vivasThatHasGroup, vivasIndependent) = acc

      // find the vivas that have direct resource dependencies
      val directVivas = vivasThatHasDirectDependencies(viva, vivas.toSet)

      directVivas match
        // case -> no direct dependencies, viva is independent
        case set if set.isEmpty =>
          (groupAcc, vivasThatHasGroup, vivasIndependent + viva)

        // case -> the vivas has direct dependencies
        case directVivas =>
          // filter direct vivas that haven't been grouped yet
          val directVivasWithoutGroup = directVivas.filter(v => !vivasThatHasGroup.contains(v.title))

          // include the current viva if it hasn't been grouped yet
          val allVivasThatNeedGroup = if (!vivasThatHasGroup.contains(viva.title)) directVivasWithoutGroup + viva else directVivasWithoutGroup

          // add vivas to groups
          val updatedGroups = addVivasToGroups(groupAcc, allVivasThatNeedGroup)

          // update vivasThatHasGroup
          val newVivasThatHasGroup = allVivasThatNeedGroup.foldLeft(vivasThatHasGroup) { (acc, newViva) =>
            acc + newViva.title
          }

          // merge groups
          val mergedGroups = mergeGroups(updatedGroups, directVivas, viva, vivaJuryToResourceIdList(viva.jury).toSet)

          (mergedGroups, newVivasThatHasGroup, vivasIndependent)
    }
    (vivasDependencyGroup, vivasIndependent)

  /**
   * Given a list of independent vivas give the optimize ScheduleViva of each one
   *
   * @param vivas list of independent vivas
   * @return list of ScheduleViva
   */
  def optimizeIndependentVivas(vivas: List[(Viva, List[AvailabilitySchedule])]): List[ScheduleViva] =
    vivas.foldLeft(List.empty[ScheduleViva]) { (acc, e) =>
      val (viva, availabilitiesList) = e

      availabilitiesList.sortWith((a, b) => a.preference > b.preference).headOption match
        case Some(availability) =>
          acc ::: List(ScheduleViva(viva, availability))
        case None =>
          acc
    }

  /**
   * Transforms a agenda into a List that contains the viva and the respective intervals where it's possible to occur
   *
   * @param agenda the target agenda
   * @return list that maps (Viva, respective possible availabilities)
   */
  private def transformAgendaIntoVivasWithPossibleAvailabilities(agenda: Agenda): Result[List[(Viva, List[AvailabilitySchedule])]] =

    def transformSingleViva(viva: Viva): Result[(Viva, List[AvailabilitySchedule])] =
      // gets a list containing the list of availabilities per resource of the viva jury
      val resourceAvailabilities = vivaJuryToResourceList(viva.jury).foldLeft(List[List[AvailabilityAgenda]]()) { (acc, resource) =>
        sortList(resource.availabilities) :: acc
      }
      val availabilities = resourceAvailabilities.map(r => r.map(r => parseAvailabilityAgenda(r)))
      availabilities.headOption match
        case Some(x) => Right((viva, findAvailabilitiesForViva(availabilities.drop(1), x, agenda.duration)))
        case _ => Left(DomainError.ImpossibleSchedule)

    agenda.vivas.foldLeft(Right(Nil): Result[List[(Viva, List[AvailabilitySchedule])]]) { (acc, viva) =>
      for {
        c <- transformSingleViva(viva)
        newAcc <- acc
      } yield c :: newAcc
    }

  /**
   * Finds disputed intervals for each Viva based on given time intervals
   *
   * @param vivasWithIntervals the list of tuples where each tuple contains a Viva and its list of possible intervals
   * @return the list of tuples where each tuple contains a Viva and the list of TimeIntervals that match the disputed
   *         intervals
   */
  def findDisputedIntervals(vivasWithIntervals: List[(Viva, List[TimeInterval])]): List[(Viva, List[TimeInterval])] =

    def createIntervalsBasedOnTimes(vivasWithIntervals: List[(Viva, List[TimeInterval])]): Result[List[TimeInterval]] =
      val orderedTimes = vivasWithIntervals.foldLeft(Set.empty[IntervalDateTime]) { (acc, e) =>
        val (_, timeIntervals) = e
        timeIntervals.foldLeft(acc) { (innerAcc, ti) =>
          innerAcc + ti.start + ti.end
        }
      }.toList.sortBy(_.to)

      val resultsList: List[Result[TimeInterval]] = orderedTimes.sliding(2).toList.map { pair =>
        val List(start, end) = pair
        TimeInterval.from(start, end).fold(e => Left(e), ti => Right(ti))
      }

      sequence(resultsList)

    def findIntervalsBasedOnTimesThatMatchesViva(vivaIntervals: List[TimeInterval], intervalsBasedOnTimes: List[TimeInterval]): List[TimeInterval] =
      intervalsBasedOnTimes.filter { interval =>
        vivaIntervals.exists(vivaInterval => isInnerInterval(interval, vivaInterval))
      }

    createIntervalsBasedOnTimes(vivasWithIntervals) match
      case Left(e) => List()
      case Right(intervalsBasedOnTimes) =>
        vivasWithIntervals.foldLeft(List.empty[(Viva, List[TimeInterval])]) { (acc, e) =>
          val (viva, timeIntervals) = e
          acc ::: List((viva, findIntervalsBasedOnTimesThatMatchesViva(timeIntervals, intervalsBasedOnTimes)))
        }

  def intraVivaCombine(vivas: List[(Viva, List[TimeInterval])], duration: VivaDuration): List[(Viva, List[List[TimeInterval]])] =

    @tailrec
    def combine(intervals: List[TimeInterval], acc: List[List[TimeInterval]]): List[List[TimeInterval]] =

      def combinationIsValid(intervals: List[TimeInterval]) =
        val result = for
          head <- intervals.headOption
          last <- intervals.lastOption
        yield duration.fitsInto(last.start, head.end)

        result.fold(false)(r => r)

      @tailrec
      def filterIntervals(intervals: List[TimeInterval], acc: List[TimeInterval]): List[TimeInterval] =
        intervals match
          case Nil => acc
          case h :: t => t.headOption match
            case Some(secondElm) =>
              if (IntervalService.meets(h, secondElm))
                filterIntervals(t, h :: acc)
              else
                h :: acc
            case None => h :: acc

      intervals match
        case Nil => acc
        case h :: t =>
          val filtered = filterIntervals(intervals, List()).reverse

          combine(t, filtered.foldLeft(List[List[TimeInterval]](), List[TimeInterval]())((acc, e) =>
            val combination = e :: acc._2

            if (combinationIsValid(combination))
              (combination :: acc._1, e :: acc._2)
            else
              (acc._1, e :: acc._2)

          )._1 ::: acc)

    vivas.foldLeft(List[(Viva, List[List[TimeInterval]])]())((acc, e) =>
      (e._1, combine(e._2, List[List[TimeInterval]]()).map(_.reverse).reverse) :: acc)

  def interVivaCombine(space: List[(Viva, List[List[TimeInterval]])], duration: VivaDuration): List[List[(Viva, TimeInterval)]] =

    @tailrec
    def combine(space: List[(Viva, List[List[TimeInterval]])], acc: List[List[(Viva, List[TimeInterval])]], resultAllocation: List[List[(Viva, TimeInterval)]]):
    (List[List[(Viva, List[TimeInterval])]], List[List[(Viva, TimeInterval)]]) =

      space match
        case Nil => (acc, resultAllocation)
        case head :: tail =>
          val newAcc = head._2.flatMap(interval => acc.map(comb => (head._1, interval) :: comb))

          val (validCombinations, allocations) = newAcc.foldLeft((List[List[(Viva, List[TimeInterval])]](), List[List[(Viva, TimeInterval)]]()))((acc, e) => {
            val sortedCombElements = e.sortBy(combElm => (combElm._2.headOption, combElm._2.size))(
              Ordering.Tuple2(
                Ordering.Option(Ordering.fromLessThan((x: TimeInterval, y: TimeInterval) => x < y)),
                Ordering.Int
              )
            )

            allocateCombination(sortedCombElements, Map(), duration, Right(List())) match
              case Right(allocation) => (e :: acc._1, allocation :: acc._2)
              case _ => acc
          })

          combine(tail, validCombinations.take(40000), allocations)

    combine(space, List(List()), List(List()))._2

  @tailrec
  def allocateCombination(combinationElements: List[(Viva, List[TimeInterval])],
                          searchTable: Map[TimeInterval, List[(Viva, IntervalDateTime)]],
                          duration: VivaDuration,
                          result: Result[List[(Viva, TimeInterval)]]): Result[List[(Viva, TimeInterval)]] =

    combinationElements match
      case Nil => Left(DomainError.IntervalConflict)

      case head :: Nil =>
        allocateViva(head, searchTable, duration)._2.fold(_ => Left(DomainError.IntervalConflict), interval => {
          result.fold(_ => Left(DomainError.IntervalConflict), result => Right((head._1, interval) :: result))
        })

      case head :: tail =>
        allocateViva(head, searchTable, duration) match
          case (_, Left(_)) => Left(DomainError.IntervalConflict)
          case (searchTable, Right(interval)) =>
            result match
              case Left(_) => Left(DomainError.IntervalConflict)
              case Right(r) => allocateCombination(tail, searchTable, duration, Right((head._1, interval) :: r))

  def allocateViva(viva: (Viva, List[TimeInterval]), searchTable: Map[TimeInterval, List[(Viva, IntervalDateTime)]],
                   duration: VivaDuration): (Map[TimeInterval, List[(Viva, IntervalDateTime)]], Result[TimeInterval]) =

    def updateSearchTable(interval: TimeInterval): Map[TimeInterval, List[(Viva, IntervalDateTime)]] =
      viva._2.findLast(int => (interval.end.isBefore(int.end) && interval.end.isAfter(int.start))
          || interval.end.isEqual(int.start) || interval.end.isEqual(int.end))

        .fold(searchTable)(targetInt => {
          searchTable.get(targetInt).fold(searchTable + (targetInt -> List((viva._1, interval.end))))(value => {
            searchTable + (targetInt -> ((viva._1, interval.end) :: value))
          })
        })

    @tailrec
    def searchForAllocation(vivaIntervals: List[TimeInterval], result: Result[TimeInterval]): (Map[TimeInterval,
      List[(Viva, IntervalDateTime)]], Result[TimeInterval]) =

      vivaIntervals match
        case Nil => (searchTable, result)

        case head :: Nil =>
          searchTable.get(head) match
            case Some(value) => allocate(head, value, duration, true) // pode ou não conseguir alocar por causa dos conflitos
            case None => // consegue alocar sempre, tem é de se verificar se o end salta para os intervalos da frente para dar update à search table
              TimeInterval.from(head.start, head.start.sum(duration))
                .fold(err => (searchTable, Left(err)), int => (updateSearchTable(int), Right(int)))

        case head :: tail =>
          searchTable.get(head) match
            case Some(value) =>
              allocate(head, value, duration, false) match
                case (searchTable, Right(interval)) => (searchTable, Right(interval)) // consegue alocar, tem é de se verificar se o end salta para os intervalos da frente para dar update na search table
                case (_, Left(DomainError.IntervalConflict)) => (searchTable, Left(DomainError.ImpossibleSchedule)) // não consegue alocar por causa de conflitos
                case (_, Left(_)) => searchForAllocation(tail, result) // não aloca porque não existem conflitos e ainda existem intervalos mais à esquerda

            case None => searchForAllocation(tail, result)

    def allocate(interval: TimeInterval, allocatedVivas: List[(Viva, IntervalDateTime)], duration: VivaDuration,
                 isLastInterval: Boolean): (Map[TimeInterval, List[(Viva, IntervalDateTime)]], Result[TimeInterval]) =

      def intervalIsValid(allocatedInterval: TimeInterval) =
        viva._2.reverse.headOption.fold(Left(DomainError.IntervalConflict))(latestAvailableInt => {
          if (allocatedInterval.end.isBefore(latestAvailableInt.end) || allocatedInterval.end.isEqual(latestAvailableInt.end))
            Right(allocatedInterval)
          else
            Left(DomainError.IntervalConflict)
        })

      val vivaResources = vivaJuryToResourceIdList(viva._1.jury)

      allocatedVivas.filter(v => {
        val currentVivaResources = vivaJuryToResourceIdList(v._1.jury)
        vivaResources.exists(r => currentVivaResources.contains(r))
      }).sortWith((v1, v2) => v1._2.isAfter(v2._2)).headOption match

        case Some(i) => // podem existir conflitos e não conseguir alocar
          TimeInterval.from(i._2, i._2.sum(duration))
            .fold(_ => (searchTable, Left(DomainError.ImpossibleSchedule)), i => intervalIsValid(i)
              .fold(_ => (searchTable, Left(DomainError.IntervalConflict)), i => (updateSearchTable(i), Right(i))))

        case None if isLastInterval => // consegue alocar sempre
          TimeInterval.from(interval.start, interval.start.sum(duration))
            .fold(_ => (searchTable, Left(DomainError.ImpossibleSchedule)), i => (updateSearchTable(i), Right(i)))

        case _ => (searchTable, Left(DomainError.ImpossibleSchedule)) // não existem conflitos e tem intervalos mais à esquerda

    searchForAllocation(viva._2.reverse, Left(DomainError.ImpossibleSchedule))


  private def getBestAllocation(vivasAgendaAvailabilities: List[(Viva, List[AvailabilitySchedule])],
                                listOfAllocations: List[List[(Viva, TimeInterval)]]): List[ScheduleViva] =

    val mapVivaToAvailabilities = vivasAgendaAvailabilities.toMap[Viva, List[AvailabilitySchedule]]

    def getScheduleViva(viva: Viva, intervalChosen: TimeInterval): Option[ScheduleViva] =
      mapVivaToAvailabilities.get(viva) match
        case None => None
        case Some(listAgendaAvailability) =>
          listAgendaAvailability.find(la => intervalsOverlap(la, intervalChosen)) match
            case None => None
            case Some(availabilityChosen) =>
              val newAvailability = AvailabilitySchedule(intervalChosen, availabilityChosen.preference)
              Some(ScheduleViva(viva, newAvailability))

    listOfAllocations.foldLeft((List[ScheduleViva](), SchedulePreference.zero)) { (acc, allocations) =>

      val scheduleCombination = allocations.foldLeft((List[ScheduleViva](), SchedulePreference.zero)) { (innerAcc, vivaAllocation) =>
        val viva = vivaAllocation._1
        val timeInterval = vivaAllocation._2

        getScheduleViva(viva, timeInterval) match
          case None => innerAcc
          case Some(scheduleViva) => (scheduleViva :: innerAcc._1, innerAcc._2 + scheduleViva.aSchedule.preference)
      }

      if scheduleCombination._2 > acc._2
      then scheduleCombination
      else acc
    }._1