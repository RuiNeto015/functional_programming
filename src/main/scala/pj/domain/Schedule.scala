package pj.domain

import pj.domain.DomainError.InvalidSchedule
import pj.domain.SimpleTypes.*

final case class Schedule private(scheduledVivas: List[ScheduleViva],
                                  preference: SchedulePreference)

object Schedule:
  def from(scheduledVivas: List[ScheduleViva], preference: SchedulePreference): Result[Schedule] =
    if (scheduledVivas.nonEmpty) Right(Schedule(scheduledVivas, preference))
    else Left(InvalidSchedule("a schedule must contain at least one scheduled viva"))