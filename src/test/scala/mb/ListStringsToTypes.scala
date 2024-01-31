package mb

import zio.{ZIO, ZIOAppDefault}

object ListStringsToTypes extends ZIOAppDefault:

  enum TypedTree:
    case 🌳
    case 🌴
    case 🌲
  override def run =
    val strings: List[String] = List("🌳", "🌴", "🌲")
    val `🪄` : String => TypedTree = {
      case "🌳" => TypedTree.🌳
      case "🌴" => TypedTree.🌴
      case "🌲" => TypedTree.🌲
    }

    val logs = strings.map(`🪄`)

    given showTree: Show[TypedTree] = {
      case TypedTree.🌳 => "🌳"
      case TypedTree.🌴 => "🌴"
      case TypedTree.🌲 => "🌲"
    }

    ZIO.debug(logs)
