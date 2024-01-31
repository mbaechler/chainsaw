package mb

import zio.{ZIO, ZIOAppDefault}

object ListCutTreesToLogs extends ZIOAppDefault:

  enum TypedTree:
    case 🌳
    case 🌴
    case 🌲

  enum Log:
    case `🪵`
  override def run =
    import Log.*
    import TypedTree.*

    val trees: List[TypedTree] = List(🌳, 🌴, 🌲)
    val `🪚` : TypedTree => List[Log] = {
      case 🌳 => List(🪵, 🪵)
      case 🌴 => List()
      case 🌲 => List(🪵, 🪵, 🪵)
    }

    val logs = trees.flatMap(`🪚`)

    ZIO.debug(logs.mkString(""))
