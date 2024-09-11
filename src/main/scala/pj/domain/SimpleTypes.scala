package pj.domain

import pj.domain.DomainError.*
import pj.domain.Result

import java.time.{DateTimeException, Duration, LocalDateTime, LocalTime, ZoneOffset}
import java.time.format.DateTimeParseException
import scala.annotation.targetName


object SimpleTypes:
  opaque type ResourceId = String

  object ResourceId:
    def from(rId: String): Result[ResourceId] =
      if (rId.isBlank || !rId.matches("[TE][0-9]{3}")) Left(InvalidResourceId(rId)) else Right(rId)

    extension (rId: ResourceId)
      def isEqual(r: ResourceId): Boolean = r.equals(rId)

  opaque type ResourceName = String

  object ResourceName:
    def from(rName: String): Result[ResourceName] =
      if (rName.isBlank) Left(InvalidResourceName(rName)) else Right(rName)

    extension (rName: ResourceName)
      def isEqual(rn: ResourceName): Boolean = rn.equals(rName)

  opaque type IntervalDateTime = LocalDateTime

  object IntervalDateTime:

    def from(localDateTime: LocalDateTime): IntervalDateTime =
      localDateTime

    def from(domainError: String => DomainError)(long: Long): Result[IntervalDateTime] =
      try
        val dateTime = LocalDateTime.ofEpochSecond(long, 0, java.time.ZoneOffset.UTC)
        if (dateTime.isBefore(LocalDateTime.MIN) || dateTime.isAfter(LocalDateTime.MAX))
          Left(domainError(long.toString))
        else
          Right(dateTime)
      catch
        case e: DateTimeException => Left(domainError(long.toString))

    def from(domainError: String => DomainError)(aDateTimeStr: String): Result[IntervalDateTime] =
      try
        Right(LocalDateTime.parse(aDateTimeStr))
      catch
        case e: DateTimeParseException => Left(domainError(aDateTimeStr))

  extension (aDateTime: IntervalDateTime)
    def to: LocalDateTime = aDateTime
    def toSeconds: Long = aDateTime.toEpochSecond(ZoneOffset.UTC)
    def isBefore(a: IntervalDateTime): Boolean = aDateTime.isBefore(a)
    def isEqual(a: IntervalDateTime): Boolean = aDateTime.isEqual(a)
    def isAfter(a: IntervalDateTime): Boolean = aDateTime.isAfter(a)
    def sum(a: VivaDuration): IntervalDateTime = aDateTime.to.plusHours(a.getHour).plusMinutes(a.getMinute).plusSeconds(a.getSecond)

  opaque type AvailabilityAgendaPreference = Int

  object AvailabilityAgendaPreference:
    def from(domainError: String => DomainError)(aPreferenceStr: String): Result[AvailabilityAgendaPreference] =
      try
        val preferenceInt = aPreferenceStr.toInt
        if (preferenceInt < 1 || preferenceInt > 5) Left(domainError(s"$aPreferenceStr")) else Right(preferenceInt)
      catch
        case e: NumberFormatException => Left(domainError(s"$aPreferenceStr"))

    extension (A: AvailabilityAgendaPreference)
      @targetName("AvailabilityPreferencePlus")
      def +(a: AvailabilityAgendaPreference): SchedulePreference = a + A
      def to(): SchedulePreference = A
      @targetName("AvailabilityPreferenceHigher")
      def >(a: AvailabilityAgendaPreference): Boolean = A > a

  opaque type VivaTitle = String

  object VivaTitle:
    def from(vTile: String): Result[VivaTitle] =
      if (vTile.isBlank) Left(InvalidVivaTitle(vTile)) else Right(vTile)

  opaque type VivaStudent = String

  object VivaStudent:
    def from(vStudent: String): Result[VivaStudent] =
      if (vStudent.isBlank) Left(InvalidVivaStudent(vStudent)) else Right(vStudent)

  opaque type SchedulePreference = Int

  object SchedulePreference:
    def zero: SchedulePreference = 0

    def from(domainError: String => DomainError)(sPreferenceInt: Int): Result[SchedulePreference] =
      if (sPreferenceInt < 0) Left(domainError(s"$sPreferenceInt")) else Right(sPreferenceInt)

    extension (S: SchedulePreference)
      @targetName("SchedulePreferencePlus")
      def +(s: AvailabilityAgendaPreference | SchedulePreference): SchedulePreference = s + S
      @targetName("SchedulePreferenceMinus")
      def -(s: SchedulePreference): SchedulePreference = s - S
      @targetName("SchedulePreferenceGreater")
      def >(s: SchedulePreference): Boolean = S > s
      @targetName("SchedulePreferenceEquals")
      def ==(s: SchedulePreference): Boolean = S == s

  opaque type VivaDuration = LocalTime

  object VivaDuration:
    def from(vDuration: String): Result[VivaDuration] =
      try
        Right(LocalTime.parse(vDuration))
      catch
        case e: DateTimeParseException => Left(InvalidVivaDuration(vDuration))

    extension (v: VivaDuration)
      def toSeconds: Int = v.toSecondOfDay
      def fitsInto(a: IntervalDateTime, b: IntervalDateTime): Boolean =
        try Math.abs(Duration.between(a, b).toSeconds) - v.toSecondOfDay >= 0
        catch case _ => false