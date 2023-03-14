package wacc

object ArchitectureType extends Enumeration {
    type Architecture = Value
    val ARM11, X86 = Value

    def getArchitecture(arch: String): Option[Architecture] = {
      arch match {
        case "arm" => Some(ARM11)
        case "x86" => Some(X86)
        case _ => None
      }
    }
  }