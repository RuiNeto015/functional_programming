package pj.domain.xml

import pj.domain.SimpleTypes.*
import pj.domain.{DomainError, Resource, Schedule, ScheduleViva}

import java.time.format.DateTimeFormatter
import scala.xml.{Elem, Node}

object DomainToXML:

  val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  def superAdvisorToXml(r: Resource): Elem =
      <supervisor name={r.rName.toString}/>

  def coAdvisorToXml(r: Resource): Elem =
      <coadvisor name={r.rName.toString}/>

  def vivaToXml(sViva: ScheduleViva): Elem =
    <viva student={sViva.viva.student.toString} title={sViva.viva.title.toString} start={sViva.aSchedule.start.to.format(formatter)} end={sViva.aSchedule.end.to.format(formatter)} preference={sViva.aSchedule.preference.toString}>
      <president name={sViva.viva.jury.president.rName.toString}/>
      <advisor name={sViva.viva.jury.advisor.rName.toString}/>
      {sViva.viva.jury.coAdvisors.map(coAdvisorToXml)}
      {sViva.viva.jury.supervisors.map(superAdvisorToXml)}
    </viva>

  def scheduleToXml(xsd: String)(schedule: Schedule): Elem =
    <schedule xsi:noNamespaceSchemaLocation={xsd} totalPreference={schedule.preference.toString} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      {schedule.scheduledVivas.map(vivaToXml)}
    </schedule>

  def errorToXml(xsd: String)(error: DomainError): Elem =
      <error xsi:noNamespaceSchemaLocation={xsd} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" message={error.toString}/>

