package pj.domain.schedule

import pj.domain.DomainError.InvalidTimeInterval
import pj.domain.SimpleTypes.{AvailabilityAgendaPreference, IntervalDateTime, ResourceId, SchedulePreference, VivaDuration}
import pj.domain.services.IntervalService

import scala.xml.Elem
import pj.domain.{Agenda, AvailabilityAgenda, AvailabilitySchedule, DomainError, Interval, Result, ScheduleViva, TimeInterval, Viva, VivaJury, Schedule as ScheduleDomain}
import pj.domain.xml.{DomainToXML, XMLToDomain}
import pj.domain.services.ScheduleService.*
import pj.domain.services.ListService.sortList

import scala.annotation.tailrec


object ScheduleMS01 extends Schedule:

  /**
   * Algorithm entry point
   *
   * @param xml the xml root element
   * @return the result xml element of the algorithm
   */
  def create(xml: Elem): Result[Elem] =
    val agenda = XMLToDomain.parseToDomain(xml)
    val writer = DomainToXML.scheduleToXml("../../schedule.xsd")

    agenda match
      case Right(a) => execScheduleMS01(a) match
        case Right(sd) => Right(writer(sd)) // algorithm output if everything went fine
        case Left(e) => Left(e)
      case Left(e) => Left(e)

  def execScheduleMS01(agenda: Agenda): Result[ScheduleDomain] =
    val hm = initMap(agenda.vivas)
    val result = iterateVivas(agenda.duration, agenda.vivas, (List(), SchedulePreference.zero, hm, None))

    // verify the presence of a domain error during iterateVivas() call
    result._4 match
      case Some(e) => Left(e) // some domain error was found during iterateVivas()
      case _ => ScheduleDomain.from(result._1.sortBy(sv => sv.aSchedule), result._2)

  /**
   * Iterates the vivas in a recursive way and carries data along the recursive calls
   *
   * @param d   duration of the vivas
   * @param l   list of vivas to iterate
   * @param acc the data that has to be carried during the iterations. The 4-tuple carries respectively:
   *            - list of scheduled vivas
   *            - schedule preference
   *            - map that holds the updated resource's availabilities considering the already scheduled vivas
   *            - Serves as an indicator that during the iteration a domain error was created
   * @return the carried data along the recursive calls
   */
  @tailrec
  private def iterateVivas(d: VivaDuration, l: List[Viva], acc: (List[ScheduleViva], SchedulePreference, Map[ResourceId, List[AvailabilityAgenda]], Option[DomainError])):
  (List[ScheduleViva], SchedulePreference, Map[ResourceId, List[AvailabilityAgenda]], Option[DomainError]) =

    l match
      case h :: Nil =>
        scheduleViva(buildAvailabilitiesList(h.jury, acc._3), d) match
          case Right(as) => // is possible to schedule the viva
            (ScheduleViva(h, as) +: acc._1, acc._2 + as.preference, rebuildAvailabilitiesMap(as, h.jury, acc._3, d), acc._4)
          case Left(e) => (acc._1, acc._2, acc._3, Some(e)) // is not possible to schedule the viva
      case h :: t =>
        scheduleViva(buildAvailabilitiesList(h.jury, acc._3), d) match
          case Right(as) => // is possible to schedule the viva
            iterateVivas(d, t, (ScheduleViva(h, as) +: acc._1, acc._2 + as.preference, rebuildAvailabilitiesMap(as, h.jury, acc._3, d), acc._4))
          case Left(e) => (acc._1, acc._2, acc._3, Some(e)) // is not possible to schedule the viva
      case _ => acc


  /**
   * Finds the earliest time interval for which a viva can occur
   *
   * @param a all availabilities of the viva's resources
   * @param d the vivas duration
   * @return a possible time interval for which the viva can occur
   */
  def scheduleViva(a: List[List[AvailabilityAgenda]], d: VivaDuration): Result[AvailabilitySchedule] =
    val availabilities = a.map(a => a.map(a => parseAvailabilityAgenda(a)))

    val intersections = availabilities.headOption match
      case Some(x) => Right(findAvailabilitiesForViva(availabilities.drop(1), x, d))
      case _ => Left(DomainError.ImpossibleSchedule)

    intersections match
      case Right(x) => x.headOption.fold(Left(DomainError.ImpossibleSchedule))(i => Right(extractEarlierInterval(i, d)))
      case _ => Left(DomainError.ImpossibleSchedule)

  /**
   * Initializes the map that will hold the updated resource's availabilities considering the already scheduled vivas
   *
   * @param vivas the vivas
   * @return the map
   */
  private def initMap(vivas: List[Viva]): Map[ResourceId, List[AvailabilityAgenda]] =
    vivas.foldLeft(Map[ResourceId, List[AvailabilityAgenda]]())((acc, e) => {
      acc + (e.jury.advisor.rId -> sortList(e.jury.advisor.availabilities))
        + (e.jury.president.rId -> sortList(e.jury.president.availabilities))
        ++ e.jury.coAdvisors.map(r => r.rId -> sortList(r.availabilities))
        ++ e.jury.supervisors.map(r => r.rId -> sortList(r.availabilities))
    })

  private def buildAvailabilityAgenda(idt1: IntervalDateTime, idt2: IntervalDateTime,
                                      sp: AvailabilityAgendaPreference): Result[AvailabilityAgenda] =

    TimeInterval.from(idt1, idt2).fold(e => Left(InvalidTimeInterval("Invalid Time Interval")),
      i => Right(AvailabilityAgenda(i, sp)))

  private def removeTimeIntervalFromInterval(i: AvailabilityAgenda, itr: Interval, duration: VivaDuration): List[AvailabilityAgenda] =
    (itr.start.isAfter(i.start), itr.end.isBefore(i.end)) match
      case (true, true) =>
        val x = for {
          ti1 <- buildAvailabilityAgenda(i.start, itr.start, i.preference)
          ti2 <- buildAvailabilityAgenda(itr.end, i.end, i.preference)
        } yield List(ti1, ti2)
        x.fold(e => List(), l => l.filter(a => duration.fitsInto(a.start, a.end)))
      case (true, false) =>
        buildAvailabilityAgenda(i.start, itr.start, i.preference).fold(e => List(), a => List(a)
          .filter(a => duration.fitsInto(a.start, a.end)))
      case (false, true) =>
        buildAvailabilityAgenda(itr.end, i.end, i.preference).fold(e => List(), a => List(a)
          .filter(a => duration.fitsInto(a.start, a.end)))
      case _ => List()

  /**
   * Rebuilds the availabilities map with the updated availabilities
   *
   * @param i        the interval where a viva will take place
   * @param hm       the previous map version
   * @param duration the vivas duration
   * @return the rebuilt map
   */
  private def rebuildAvailabilitiesMap(i: Interval, jury: VivaJury, hm: Map[ResourceId, List[AvailabilityAgenda]], duration: VivaDuration): Map[ResourceId, List[AvailabilityAgenda]] =
    hm.flatten((r, a) => Map(r -> a.foldLeft(List[AvailabilityAgenda]())((acc, e) => {
      val intervalIsNotEligible = !IntervalService.contains(e, i) && !IntervalService.finishedBy(e, i) && !IntervalService.equals(e, i) &&
        !IntervalService.startedBy(e, i)

      val resourceIsNotEligible = !(jury.president :: jury.advisor :: jury.coAdvisors ::: jury.supervisors).exists(re => re.rId.equals(r))

      if (intervalIsNotEligible || resourceIsNotEligible)
        acc ::: List(e)
      else
        sortList(removeTimeIntervalFromInterval(e, i, duration) ::: acc)
    }))).toMap

  /**
   * Puts all the resources availabilities lists into a list
   *
   * @param jury the viva jury (resources)
   * @param hm   the resources updated availabilities
   * @return the list of availabilities lists
   */
  private def buildAvailabilitiesList(jury: VivaJury, hm: Map[ResourceId, List[AvailabilityAgenda]]): List[List[AvailabilityAgenda]] =
    List(hm.get(jury.president.rId).fold(List())(l => l), hm.get(jury.advisor.rId).fold(List())(l => l)) :::
      jury.supervisors.map(r => hm.get(r.rId).fold(List())(l => l)) ::: jury.coAdvisors.map(r => hm.get(r.rId).fold(List())(a => a))