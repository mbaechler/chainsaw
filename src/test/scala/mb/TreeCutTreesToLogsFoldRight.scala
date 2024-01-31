package mb

import zio.{ZIO, ZIOAppDefault}

object TreeCutTreesToLogsFoldRight extends ZIOAppDefault:

  enum TypedHomes:
    case 🌳
    case 🌴
    case 🌲
    case `🏘️`
    case 🏡

  enum Log:
    case `🏘️`
    case 🏡
    case `🪵`

  enum Accumulator:
    case Wood(logs: List[Log])
    case Homes(trees: List[Tree[Log]])
    case Root(tree: Tree[Log])
  override def run =
    import Log.`🪵`
    import TypedHomes.*

    val trees: Tree[TypedHomes] = Tree(
      TypedHomes.`🏘️`,
      Tree(🏡, Tree(🌳), Tree(🌴)),
      Tree(🏡, Tree(🌲), Tree(🌲))
    )

    import Accumulator.*
    val `🪚` : (TypedHomes, List[Accumulator]) => List[Accumulator] = {
      (e, accumulator) =>
        (e, accumulator) match
          case (🌳, (w: Wood) :: others) =>
            Wood(`🪵` :: `🪵` :: w.logs) :: others

          case (🌳, others) =>
            Wood(`🪵` :: `🪵` :: Nil) :: others

          case (🌲, (w: Wood) :: others) =>
            Wood(`🪵` :: `🪵` :: `🪵` :: w.logs) :: others

          case (🌲, others) => Wood(`🪵` :: `🪵` :: `🪵` :: Nil) :: others

          case (🌴, _) => accumulator

          case (🏡, (w: Wood) :: (h: Homes) :: others) =>
            Homes(
              Tree(Log.🏡, w.logs.map(log => Tree(log))) :: h.trees
            ) :: others

          case (🏡, (h: Homes) :: others) =>
            Homes(Tree(Log.🏡) :: h.trees) :: others

          case (🏡, (w: Wood) :: others) =>
            Homes(Tree(Log.🏡, w.logs.map(log => Tree(log))) :: Nil) :: others

          case (🏡, others) => Homes(Tree(Log.🏡) :: Nil) :: others

          case (TypedHomes.`🏘️`, (h: Homes) :: Nil) =>
            Root(Tree(Log.`🏘️`, h.trees)) :: Nil

          case (TypedHomes.`🏘️`, other) => ???
    }

    val (Root(logs) :: Nil) = trees.foldRight(`🪚`, List.empty)

    given Show[TypedHomes] = {
      case TypedHomes.🌳    => "🌳"
      case TypedHomes.🌴    => "🌴"
      case TypedHomes.🌲    => "🌲"
      case TypedHomes.`🏘️` => "🏘️"
      case TypedHomes.🏡    => "🏡"
    }

    given Show[Log] = {
      case Log.`🪵`  => "🪵"
      case Log.`🏘️` => "🏘️"
      case Log.🏡    => "🏡"
    }

    ZIO.debug("before") *> ZIO.debug(show(trees)) *> ZIO.debug("after") *> ZIO
      .debug(show(logs))
