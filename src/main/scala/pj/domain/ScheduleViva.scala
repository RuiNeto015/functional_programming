package pj.domain

import pj.domain.services.VivaService.*

final case class ScheduleViva(viva: Viva,
                              aSchedule: AvailabilitySchedule)

