package pj.domain.services

import pj.domain.SimpleTypes.ResourceId
import pj.domain.{Resource, VivaJury}

object ResourcesService:
  def vivaJuryToResourceIdList(jury: VivaJury): List[ResourceId] =
    (jury.president :: jury.advisor :: jury.coAdvisors ::: jury.supervisors).map(r => r.rId)

  def vivaJuryToResourceList(jury: VivaJury): List[Resource] =
    jury.president :: jury.advisor :: jury.coAdvisors ::: jury.supervisors
