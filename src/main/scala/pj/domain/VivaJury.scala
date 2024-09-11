package pj.domain

import pj.domain.DomainError.InvalidVivaJuryConstitution
import pj.domain.services.ListService.hasDuplicates

/**
 * The viva jury is a a set of resources (teacher or external persons), each of which has a role in the viva.
 * The roles associated with the viva are:
 *   - one jury President (mandatory) - it can be a teacher
 *   - one Advisor (mandatory) - it can be a teacher
 *   - zero or more Co-advisors (optional) - it can be a teacher or external
 *   - zero or more Supervisors (optional) - it can be a external
 *
 * Note: Each teacher, external person can have only one role.
 */
final case class VivaJury private(president: Teacher,
                                  advisor: Teacher,
                                  coAdvisors: List[Resource],
                                  supervisors: List[External])


object VivaJury:

  def from(president: Teacher, advisor: Teacher, coAdvisors: List[Resource], supervisors: List[External]): Result[VivaJury] =
    if (hasDuplicates(president :: advisor :: coAdvisors ::: supervisors))
      Left(InvalidVivaJuryConstitution("there is duplicates of resources on the different positions of the jury viva"))
    else
      Right(VivaJury(president, advisor, coAdvisors, supervisors))
