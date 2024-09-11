package pj.domain

import pj.domain.SimpleTypes.*

final case class Viva(title: VivaTitle,
                      student: VivaStudent,
                      jury: VivaJury):

  override def equals(obj: Any): Boolean = obj match
    case other: Viva => this.title == other.title
    case _ => false

