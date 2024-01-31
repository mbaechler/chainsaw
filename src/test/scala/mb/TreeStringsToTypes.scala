package mb

import zio.{ZIO, ZIOAppDefault}

object TreeStringsToTypes extends ZIOAppDefault:

  enum TypedHomes:
    case 🌳
    case 🌴
    case 🌲
    case `🏘️`
    case 🏡

  override def run = {
    val strings: Tree[String] =
      Tree(
        "🏘️",
        Tree("🏡", Tree("🌳"), Tree("🌴")),
        Tree("🏡", Tree("🌲"), Tree("🌲"))
      )

    val `🪄` : String => TypedHomes = {
      case "🌳"  => TypedHomes.🌳
      case "🌴"  => TypedHomes.🌴
      case "🌲"  => TypedHomes.🌲
      case "🏘️" => TypedHomes.`🏘️`
      case "🏡"  => TypedHomes.🏡
    }

    val logs = strings.map(`🪄`)

    given showTree: Show[TypedHomes] = {
      case TypedHomes.🌳    => "🌳"
      case TypedHomes.🌴    => "🌴"
      case TypedHomes.🌲    => "🌲"
      case TypedHomes.`🏘️` => "🏘️"
      case TypedHomes.🏡    => "🏡"
    }

    ZIO.debug(show(logs))
  }
