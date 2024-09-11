package pj.domain

import pj.domain.DomainError.InvalidTimeInterval
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, SchedulePreference}

sealed trait Interval extends Ordered[Interval]:
  val start: IntervalDateTime
  val end: IntervalDateTime


  override def compare(that: Interval): Int =
    if (start.isEqual(that.start)) 0
    else if (start.isBefore(that.start)) -1
    else 1

final case class TimeInterval private(override val start: IntervalDateTime,
                                      override val end: IntervalDateTime) extends Interval

final case class AvailabilityAgenda(timeInterval: TimeInterval,
                                    preference: AvailabilityAgendaPreference) extends Interval:
  override val start: IntervalDateTime = timeInterval.start
  override val end: IntervalDateTime = timeInterval.end

final case class AvailabilitySchedule(timeInterval: TimeInterval,
                                      preference: SchedulePreference) extends Interval:
  override val start: IntervalDateTime = timeInterval.start
  override val end: IntervalDateTime = timeInterval.end

  override def compare(that: Interval): Int =
    that match
      case thatSchedule: AvailabilitySchedule =>
        if (start.isEqual(thatSchedule.start))
          if (preference == thatSchedule.preference) 0
          else if (preference > thatSchedule.preference) 1
          else -1
        else if (start.isBefore(thatSchedule.start)) -1
        else 1
      case _ => super.compare(that)


object TimeInterval:
  def from(start: IntervalDateTime, end: IntervalDateTime): Result[TimeInterval] =
    if (start.isBefore(end)) Right(TimeInterval(start, end))
    else Left(InvalidTimeInterval("start datetime not before end datetime"))