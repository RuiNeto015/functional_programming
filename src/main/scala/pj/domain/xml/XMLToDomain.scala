package pj.domain.xml

import pj.domain.*
import pj.domain.DomainError.{InvalidPreference, XMLError}
import pj.domain.SimpleTypes.*
import pj.xml.XML.{fromAttribute, fromNode, traverse}

import scala.xml.{Elem, Node}

object XMLToDomain:

  def parseToDomain(xml: Elem): Result[Agenda] =
    for {
      aduration <- agendaDuration(xml)
      resourcesn <- fromNode(xml, "resources")
      teachersn <- fromNode(resourcesn, "teachers")
      teachers <- teachers(teachersn)
      externalsn <- fromNode(resourcesn, "externals")
      externals <- externals(externalsn)
      vivasn <- fromNode(xml, "vivas")
      vivas <- vivas(teachers, externals)(vivasn)
      agenda <- Agenda.from(aduration, vivas)
    } yield agenda

  private def agendaDuration(xml: Node): Result[VivaDuration] =
    for {
      ads <- fromAttribute(xml, "duration")
      ad <- VivaDuration.from(ads)
    } yield ad

  private def availability(xml: Node): Result[AvailabilityAgenda] =
    for {
      starts <- fromAttribute(xml, "start")
      start <- IntervalDateTime.from(XMLError.apply)(starts)
      ends <- fromAttribute(xml, "end")
      end <- IntervalDateTime.from(XMLError.apply)(ends)
      tInterval <- TimeInterval.from(start, end)
      preferences <- fromAttribute(xml, "preference")
      preference <- AvailabilityAgendaPreference.from(InvalidPreference.apply)(preferences)
    } yield AvailabilityAgenda(tInterval, preference)

  private def availabilities(xml: Node): Result[List[AvailabilityAgenda]] =
    traverse(xml \ "availability", availability)

  private def teacher(xml: Node): Result[Teacher] =
    for {
      ids <- fromAttribute(xml, "id")
      id <- ResourceId.from(ids)
      names <- fromAttribute(xml, "name")
      name <- ResourceName.from(names)
      availabilities <- availabilities(xml)
      teacher <- Teacher.from(id, name, availabilities)
    } yield teacher

  private def teachers(xml: Node): Result[List[Teacher]] =
    traverse(xml \ "teacher", teacher)

  private def external(xml: Node): Result[External] =
    for {
      ids <- fromAttribute(xml, "id")
      id <- ResourceId.from(ids)
      names <- fromAttribute(xml, "name")
      name <- ResourceName.from(names)
      availabilities <- availabilities(xml)
      external <- External.from(id, name, availabilities)
    } yield external

  private def externals(xml: Node): Result[List[External]] =
    traverse(xml \ "external", external)

  private def coadvisor(rl: List[Resource])(xml: Node): Result[Resource] =
    for {
      ids <- fromAttribute(xml, "id")
      id <- ResourceId.from(ids)
      coadvisor <- rl.find(_.rId.equals(id)).fold(Left(XMLError("Coadvisor match not found")))(coadvisor => Right(coadvisor))
    } yield coadvisor

  private def coadvisors(rl: List[Resource])(xml: Node): Result[List[Resource]] =
    val coadvisorn = xml \ "coadvisor"
    if (coadvisorn.isEmpty)
      Right(List())
    else
      traverse(coadvisorn, coadvisor(rl))

  private def supervisor(rl: List[External])(xml: Node): Result[External] =
    for {
      ids <- fromAttribute(xml, "id")
      id <- ResourceId.from(ids)
      supervisor <- rl.find(_.rId.equals(id)).fold(Left(XMLError("Supervisor match not found")))(supervisor => Right(supervisor))
    } yield supervisor

  private def supervisors(el: List[External])(xml: Node): Result[List[External]] =
    val supervisorn = xml \ "supervisor"
    if (supervisorn.isEmpty)
      Right(List())
    else
      traverse(supervisorn, supervisor(el))

  private def vivaJury(tl: List[Teacher], el: List[External])(xml: Node): Result[VivaJury] =
    for {
      presidentn <- fromNode(xml, "president")
      presidentids <- fromAttribute(presidentn, "id")
      presidentid <- ResourceId.from(presidentids)
      president <- tl.find(_.rId.equals(presidentid)).fold(Left(XMLError("President match not found")))(president => Right(president))
      advisorn <- fromNode(xml, "advisor")
      advisorids <- fromAttribute(advisorn, "id")
      advisorid <- ResourceId.from(advisorids)
      advisor <- tl.find(_.rId.equals(advisorid)).fold(Left(XMLError("Advisor match not found")))(advisor => Right(advisor))
      coadvisors <- coadvisors(tl ++ el)(xml)
      supervisors <- supervisors(el)(xml)
      vivajury <- VivaJury.from(president, advisor, coadvisors, supervisors)
    } yield vivajury

  private def viva(tl: List[Teacher], el: List[External])(xml: Node): Result[Viva] =
    for {
      titles <- fromAttribute(xml, "title")
      title <- VivaTitle.from(titles)
      students <- fromAttribute(xml, "student")
      student <- VivaStudent.from(students)
      vivaJury <- vivaJury(tl, el)(xml)
    } yield Viva(title, student, vivaJury)

  private def vivas(tl: List[Teacher], el: List[External])(xml: Node): Result[List[Viva]] =
    traverse(xml \ "viva", viva(tl, el))