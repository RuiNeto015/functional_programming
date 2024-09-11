package pj.domain.services

import pj.domain.Interval
import pj.domain.SimpleTypes.VivaDuration
import pj.domain.services.ListService.*

/**
 * Functionality related to Allen's Interval Algebra
 * source: https://ics.uci.edu/~alspaugh/cls/shr/allen.html
 */
object IntervalService:

  def precedes(a: Interval, b: Interval): Boolean = a.start.isBefore(b.start) && a.end.isBefore(b.start)

  def meets(a: Interval, b: Interval): Boolean = a.end.isEqual(b.start)

  def overlaps(a: Interval, b: Interval): Boolean = a.start.isBefore(b.start) && a.end.isAfter(b.start) && b.end.isAfter(a.end)

  def finishedBy(a: Interval, b: Interval): Boolean = a.start.isBefore(b.start) && a.end.isEqual(b.end)

  def contains(a: Interval, b: Interval): Boolean = a.start.isBefore(b.start) && a.end.isAfter(b.end)

  def starts(a: Interval, b: Interval): Boolean = a.start.isEqual(b.start) && a.end.isBefore(b.end)

  def equals(a: Interval, b: Interval): Boolean = a.start.isEqual(b.start) && a.end.isEqual(b.end)

  //converse
  def startedBy(a: Interval, b: Interval): Boolean = starts(b, a)

  def during(a: Interval, b: Interval): Boolean = contains(b, a)

  def finishes(a: Interval, b: Interval): Boolean = finishedBy(b, a)

  def overlappedBy(a: Interval, b: Interval): Boolean = overlaps(b, a)

  def metBy(a: Interval, b: Interval): Boolean = meets(b, a)

  def precededBy(a: Interval, b: Interval): Boolean = precedes(b, a)

  /**
   * Given a list of Intervals, checks if they have intersections conflicts.
   * E.g: A(10h00-11h00), B(10h30-12h00) -> has conflicts
   * E.g: A(10h00-11h00), B(11h00-12h00) -> has no conflicts
   *
   * @param intervalList List of intervals
   * @return Boolean saying if has or not conflicts
   */
  def hasIntersectionsConflicts(intervalList: List[Interval]): Boolean =
    val orderedList = ListService.sortList(intervalList)
    val hasInvalidIntervals: Boolean = orderedList.sliding(2).exists:
      case List(a, b) => !IntervalService.precedes(a, b) && !IntervalService.meets(a, b)
      case List(_) => false // only one element
      case _ => false // empty list
    hasInvalidIntervals

  /**
   * Verifies if the interval has the exact duration given
   *
   */
  def intervalHasExactDuration(interval: Interval, duration: VivaDuration): Boolean =
    interval.end.toSeconds - interval.start.toSeconds == duration.toSeconds

  /**
   * Checks if the intervalA overlaps with interval B
   *
   */
  def intervalsOverlap(intervalA: Interval, intervalB: Interval): Boolean =
    !(precedes(intervalA, intervalB) ||
      meets(intervalA, intervalB) ||
      metBy(intervalA, intervalB) ||
      precededBy(intervalA, intervalB))
  
  def allIntervalsDontOverlap(intervalList: List[Interval]): Boolean =
    intervalList.combinations(2).forall:
      case List(intervalA, intervalB) => !intervalsOverlap(intervalA, intervalB)
      case _ => true

  /**
   * Checks if the intervalA is inner interval B
   *
   */
  def isInnerInterval(intervalA: Interval, intervalB: Interval): Boolean =
    starts(intervalA, intervalB) || 
      equals(intervalA, intervalB) || 
      during(intervalA, intervalB) || 
      finishes(intervalA, intervalB)