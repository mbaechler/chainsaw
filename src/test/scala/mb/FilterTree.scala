package mb

import zio.{ZIO, ZIOAppDefault}

object FilterTree extends ZIOAppDefault:

  enum TypedHomes:
    case 🌳
    case 🌴
    case 🌲
    case `🏘️`
    case 🏡

  override def run =
    import TypedHomes.*

    val tree: Tree[TypedHomes] = Tree(
      TypedHomes.`🏘️`,
      Tree(🏡, Tree(🌳), Tree(🌴)),
      Tree(🏡, Tree(🌲), Tree(🌲))
    )

    val filteredTrees = tree.filter {
      case 🌳 | 🌴 | 🌲 => false
      case `🏘️` | 🏡   => true
    }.get

    val filteredHomes = tree.filter {
      case 🏡                   => false
      case `🏘️` | 🌳 | 🌴 | 🌲 => true
    }.get

    given Show[TypedHomes] = {
      case TypedHomes.🌳    => "🌳"
      case TypedHomes.🌴    => "🌴"
      case TypedHomes.🌲    => "🌲"
      case TypedHomes.`🏘️` => "🏘️"
      case TypedHomes.🏡    => "🏡"
    }

    ZIO.debug("original") *> ZIO.debug(show(tree))
      *> ZIO.debug("without trees") *> ZIO.debug(show(filteredTrees))
      *> ZIO.debug("without homes") *> ZIO.debug(show(filteredHomes))
