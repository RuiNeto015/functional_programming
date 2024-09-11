package pj.domain

import pj.domain.SimpleTypes.VivaDuration
import pj.domain.DomainError.*
import pj.domain.services.ListService.*

final case class Agenda private(duration: VivaDuration,
                                vivas: List[Viva])

object Agenda:
  def from(duration: VivaDuration, vivas: List[Viva]): Result[Agenda] =
    //Right(Agenda(duration, vivas))
    if (vivas.isEmpty) Left(InvalidVivasList("Vivas list is empty"))
    else if (hasDuplicatedValue(vivas, _.student)) Left(InvalidVivasList("Vivas list has duplicated students ids"))
    else if (hasDuplicatedValue(vivas, _.title)) Left(InvalidVivasList("Vivas list has duplicated titles"))
    else Right(Agenda(duration, vivas))