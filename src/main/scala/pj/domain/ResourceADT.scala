package pj.domain

import pj.domain.*
import pj.domain.DomainError.{InvalidExternalAvailability, InvalidTeacherAvailability}
import pj.domain.SimpleTypes.{ResourceId, ResourceName}
import pj.domain.services.IntervalService.hasIntersectionsConflicts

import scala.util.Right

sealed trait Resource:
  val rId: ResourceId
  val rName: ResourceName
  val availabilities: List[AvailabilityAgenda]

  override def equals(obj: Any): Boolean = obj match
    case other: Resource => this.rId.isEqual(other.rId)
    case _ => false

final case class Teacher private(override val rId: ResourceId, override val rName: ResourceName, override val availabilities: List[AvailabilityAgenda]) extends Resource

final case class External private(override val rId: ResourceId, override val rName: ResourceName, override val availabilities: List[AvailabilityAgenda]) extends Resource

private object ResourceOps:
  def isResourceValid(domainError: String => DomainError)(rId: ResourceId, rName: ResourceName, availabilities: List[AvailabilityAgenda]): Result[Unit] =
    if (availabilities.isEmpty) Left(domainError("empty availabilities"))
    else if (hasIntersectionsConflicts(availabilities)) Left(domainError(availabilities.toString()))
    else Right(())

object Teacher:
  def from(rId: ResourceId, rName: ResourceName, availabilities: List[AvailabilityAgenda]): Result[Teacher] =
    ResourceOps.isResourceValid(InvalidTeacherAvailability.apply)(rId, rName, availabilities).map(_ => Teacher(rId, rName, availabilities))

object External:
  def from(rId: ResourceId, rName: ResourceName, availabilities: List[AvailabilityAgenda]): Result[External] =
    ResourceOps.isResourceValid(InvalidExternalAvailability.apply)(rId, rName, availabilities).map(_ => External(rId, rName, availabilities))