package pj.domain


type Result[A] = Either[DomainError, A]

enum DomainError:
  case IOFileProblem(error: String)
  case XMLError(error: String)
  case InvalidResourceId(error: String)
  case InvalidResourceName(error: String)
  case InvalidTeacherAvailability(error: String)
  case InvalidExternalAvailability(error: String)
  case InvalidTimeInterval(error: String)
  case IntervalConflict
  case InvalidVivaTitle(error: String)
  case InvalidVivaStudent(error: String)
  case InvalidVivaJuryConstitution(error: String)
  case InvalidSchedule(error: String)
  case InvalidVivaDuration(error: String)
  case InvalidVivasList(error: String)
  case InvalidResourcesList(error: String)
  case InvalidPreference(error: String)
  case ImpossibleSchedule
