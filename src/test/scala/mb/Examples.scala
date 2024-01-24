package mb

import mb.TreeStringsToTypes.TypedHomes.`🏘️`
import zio.{ZIO, ZIOAppDefault}

trait Show[T]:
  def show(t: T): String
def show[T: Show](tree: Tree[T], indent: Int = 1): String =
  summon[Show[T]].show(tree.label)
    ++ "\n"
    ++ tree.children
      .map("\\--" ++ show(_, indent + 3))
      .map(_.indent(indent))
      .mkString("")

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
