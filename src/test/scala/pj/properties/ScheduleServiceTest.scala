package pj.properties

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.schedule.ScheduleMS03.execScheduleMS03
import pj.domain.schedule.ScheduleMS01.execScheduleMS01

class ScheduleServiceTest extends AnyFunSuite:

  test("Property-based test for ScheduleService - MS01"):
    val props = new ScheduleServiceProperties(execScheduleMS01)
    props.check()

  test("Property-based test for ScheduleService - MS03 "):
    val props = new ScheduleServiceProperties(execScheduleMS03)
    props.check()