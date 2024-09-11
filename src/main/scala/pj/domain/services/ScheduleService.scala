package pj.domain.services

import pj.domain.*
import pj.domain.SimpleTypes.*
import pj.domain.services.IntervalService.*
import pj.domain.services.ResourcesService.{vivaJuryToResourceIdList, vivaJuryToResourceList}

import scala.annotation.tailrec

object ScheduleService:

  /**
   * Finds all the possible intersections between two lists of availabilities
   *
   * @param a    a list of availabilities
   * @param b    a list of availabilities
   * @param d    the vivas duration
   * @param last flag that indicates if it's the last list, meaning that there is no need to calculate intersections after
   *             finding the first one
   * @return list of all intersections between two lists of availabilities
   */
  private def intersections(a: List[AvailabilitySchedule], b: List[AvailabilitySchedule],
                            d: VivaDuration, last: Boolean): List[AvailabilitySchedule] =

    /**
     * Calculates a possible intersection between the 'e' and 'h' availabilities and transforms the fold accumulators by
     * appending the possible intersection to acc1 and dropping the 'h' from the acc2 if this can't be intersected with
     * the following fold elements
     *
     * @param e    the fold element
     * @param h    the element to intersect with 'e'
     * @param acc1 list of intersections
     * @param acc2 list of elements to intersect with
     * @return the transformed 'acc1' and 'acc2'
     */
    def iter(e: AvailabilitySchedule, h: AvailabilitySchedule, acc1: List[AvailabilitySchedule],
             acc2: List[AvailabilitySchedule]): (List[AvailabilitySchedule], List[AvailabilitySchedule]) =

      def buildIntersection(s: IntervalDateTime, e: IntervalDateTime, p: SchedulePreference): Option[AvailabilitySchedule] =
        TimeInterval.from(s, e).fold(_ => None, i => Some(AvailabilitySchedule(i, p)))

      def calculateIntersection(a: AvailabilitySchedule, b: AvailabilitySchedule): Option[AvailabilitySchedule] =
        if (precedes(a, b) || meets(a, b) || metBy(a, b) || precededBy(a, b))
          None
        else if (overlaps(a, b) && d.fitsInto(b.start, a.end))
          buildIntersection(b.start, a.end, a.preference + b.preference)
        else if ((finishedBy(a, b) || contains(a, b) || IntervalService.equals(a, b) || startedBy(a, b)) && d.fitsInto(b.start, b.end))
          Some(AvailabilitySchedule(b.timeInterval, a.preference + b.preference))
        else if ((starts(a, b) || during(a, b) || finishes(a, b)) && d.fitsInto(a.start, a.end))
          Some(AvailabilitySchedule(a.timeInterval, a.preference + b.preference))
        else if (overlappedBy(a, b) && d.fitsInto(a.start, b.end))
          buildIntersection(a.start, b.end, a.preference + b.preference)
        else None

      // a finishes after b
      def excludeFromNextIteration(a: Interval, b: Interval): Boolean =
        a.end.isAfter(b.end) || a.end.isEqual(b.end) || !d.fitsInto(a.end, b.end) // b.end - a.end < duration (40<30)

      val intersection = for {
        intersection <- calculateIntersection(e, h)
      } yield intersection

      if (excludeFromNextIteration(e, h))
        intersection match
          case Some(x) => (acc1 ::: List(x), acc2.filter(e => e != h)) // e=(08h00, 12h00)  h=(07h45, 12h00)
          case _ => (acc1, acc2.filter(e => e != h)) // e=(08h00, 12h00)  h=(06h45, 07h00)
      else
        intersection match // not break and not exclude never happens
          case Some(x) => (acc1 ::: List(x), acc2) // e=(08h00, 12h00)  h=(11h00, 14h00)
          case _ => (acc1, acc2) // e=(08h00, 12h00)  h=(12h00, 14h00)

    // a finishes before b
    def break(a: Interval, b: Interval): Boolean =
      a.end.isBefore(b.end) || a.end.isEqual(b.end) || !d.fitsInto(a.end, b.end) // a.end - b.end < duration (40<30)

    /**
     * Transforms the fold accumulators along the calculation of intersections between the 'e' and 'acc2' elements
     *
     * @param e    the element that is being intersected with the 'acc2' elements
     * @param acc1 the list of found intersections between 'e' and 'acc2' elements
     * @param acc2 the list of elements to be intersected with 'e'
     * @return the transformed fold accumulators
     */
    @tailrec
    def loop(e: AvailabilitySchedule, acc1: List[AvailabilitySchedule],
             acc2: List[AvailabilitySchedule]): (List[AvailabilitySchedule], List[AvailabilitySchedule]) =

      acc2 match
        case h :: Nil => iter(e, h, acc1, acc2)
        case h :: t =>
          if (break(e, h)) iter(e, h, acc1, acc2) // finds the intersections and breaks the loop
          else // recursive call
            val iterOutput = iter(e, h, acc1, acc2)
            loop(e, iterOutput._1, iterOutput._2)
        case _ => (acc1, acc2)

    /**
     * Iterates the list a over the list b and carries the found intersections between the two lists and a possible
     * shortened list b if the next fold element is impossible to intersect with the dropped elements
     */
    a.foldLeft((List[AvailabilitySchedule](), b))((acc, e) => {
      val lists = loop(e, acc._1, acc._2)
      (lists._1, lists._2)
    })._1

  /**
   * Finds all the possible availabilities for which a viva can occur
   *
   * @param a the list of availabilities to iterate over
   * @param i the list of availabilities to intersect with 'a'
   * @param d the vivas duration
   * @return all possible availabilities for which a viva can occur
   */
  @tailrec
  def findAvailabilitiesForViva(a: List[List[AvailabilitySchedule]], i: List[AvailabilitySchedule],
                                d: VivaDuration): List[AvailabilitySchedule] =
    a match
      case h :: Nil => intersections(i, h, d, true) // true is used to notify that it's the last list
      case h :: t => findAvailabilitiesForViva(t, intersections(i, h, d, false), d)
      case _ => i

  /**
   * Converts AvailabilityAgenda to AvailabilitySchedule
   *
   * @param a the AvailabilityAgenda
   * @return the AvailabilitySchedule
   */
  def parseAvailabilityAgenda(a: AvailabilityAgenda): AvailabilitySchedule =
    AvailabilitySchedule(a.timeInterval, a.preference.to())

  def extractEarlierInterval(as: AvailabilitySchedule, d: VivaDuration): AvailabilitySchedule =
    val s = as.start
    val e = as.start.sum(d)
    val ti = for {
      tir <- TimeInterval.from(s, e)
    } yield tir
    ti match
      case Right(i) => AvailabilitySchedule(i, as.preference)
      case Left(e) => as

  /**
   * Finds all the vivas that has a direct resource dependency with the received viva.
   *
   * @param targetViva the target viva
   * @param totalVivas the total vivas to compared
   * @return vivas that has direct resource dependency
   */
  def vivasThatHasDirectDependencies(targetViva: Viva, totalVivas: Set[Viva]): Set[Viva] =
    val targetResources = vivaJuryToResourceList(targetViva.jury)

    totalVivas.filter(v => v != targetViva).foldLeft(Set[Viva]()) { (acc, foldViva) =>
      val foldResources = vivaJuryToResourceList(foldViva.jury)
      if targetResources.exists(tr => foldResources.contains(tr)) then acc + foldViva else acc
    }

  /**
   * Creates the groups to the vivas that is necessary and adds them to the total list of groups.
   *
   * @param groupList  the actual group list
   * @param vivasToAdd the groups to create
   * @return the list of groups updated
   */
  def addVivasToGroups(groupList: List[(Set[Viva], Set[ResourceId])], vivasToAdd: Set[Viva]): List[(Set[Viva], Set[ResourceId])] =
    vivasToAdd.foldLeft(groupList) { (groupAcc, vivaToAdd) =>
      getGroupOfViva(groupAcc, vivaToAdd) match
        // if the viva not exists in the group
        case (set1, set2) if set1.isEmpty && set2.isEmpty =>
          (Set(vivaToAdd), vivaJuryToResourceIdList(vivaToAdd.jury).toSet) :: groupAcc
        // if the viva already belongs to a group
        case (vivaSet, resourceSet) =>
          (vivaSet + vivaToAdd, resourceSet ++ vivaJuryToResourceIdList(vivaToAdd.jury).toSet) :: groupAcc.filter(g => g._1 != vivaSet)
    }

  /**
   * Gets the group of the respective viva
   *
   * @param groupList  the group list
   * @param targetViva the viva to be searched
   * @return the information about the group
   */
  private def getGroupOfViva(groupList: List[(Set[Viva], Set[ResourceId])], targetViva: Viva): (Set[Viva], Set[ResourceId]) =
    groupList.find(r => r._1.contains(targetViva)) match
      case Some(x) => x
      case _ => (Set(), Set())

  /**
   * Returns the list of groups with the new merged group.
   * The logic behind the merge is to join the target viva with the vivas that has a direct dependency, but also with
   * the vivas that the direct vivas depends, e.g., if the target viva is V1 and the direct dependent viva is V2,
   * and V2 has a group with V3, what will happen is that we are going to merge all in
   * one group -> [V1, V2, V3], even with V1 not being directly dependent of V3.
   *
   * @param groupList   the actual groups
   * @param directVivas vivas that has a direct dependency
   * @param targetViva  the target viva
   * @return merged updated group
   */
  def mergeGroups(groupList: List[(Set[Viva], Set[ResourceId])],
                  directVivas: Set[Viva],
                  targetViva: Viva,
                  targetResources: Set[ResourceId],
                 ): List[(Set[Viva], Set[ResourceId])] =

    // obtain the new group to add to
    val (newGroup, updatedListOfGroups) = directVivas.foldLeft(((Set[Viva](targetViva), targetResources), groupList)):
      (acc, directViva) =>
        val ((mergedVivas, mergedResources), updatedGroupList) = acc

        getGroupOfViva(updatedGroupList, directViva) match
          case (set1, set2) if set1.isEmpty && set2.isEmpty =>
            ((mergedVivas, mergedResources), updatedGroupList)
          case (vivaSet, resourceSet) =>
            ((vivaSet ++ mergedVivas, resourceSet ++ mergedResources), updatedGroupList.filter(g => g._1 != vivaSet))

    newGroup :: updatedListOfGroups.filter(g => !g._1.contains(targetViva))

