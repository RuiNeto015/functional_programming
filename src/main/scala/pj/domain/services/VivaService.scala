package pj.domain.services

import pj.domain.Interval
import pj.domain.services.IntervalService.{meets, metBy, precededBy, precedes}

object VivaService:

  /*
   * Checks if a specific resource availability includes an interval
   *
   * @param availability the availability
   * @param interval     the interval to check for
   * @return true if the availability includes the interval otherwise false
   */
  def supportsInterval(intervalA: Interval, intervalB: Interval): Boolean =
    !precedes(intervalA, intervalB)
      && !meets(intervalA, intervalB)
      && !metBy(intervalA, intervalB)
      && !precededBy(intervalA, intervalB)