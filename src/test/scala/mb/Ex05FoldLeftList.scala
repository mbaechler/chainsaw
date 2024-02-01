package mb

import mb.TypedHomes.*

object Ex05FoldLeftList extends zio.ZIOAppDefault:

  override def run =

    val energy: TypedHomes => Int = {
      case 🌳 => 100
      case 🌴 => 5
      case 🌲 => 20
      case _  => 0
    }

    val result = List(🌳, 🌴, 🌲).foldLeft(0)((acc, tree) => energy(tree) + acc)

    zio.Console.printLine(s"$result eq. KWh")
