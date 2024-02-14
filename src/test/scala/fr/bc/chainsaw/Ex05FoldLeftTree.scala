package fr.bc.chainsaw

import fr.bc.chainsaw.TypedHomes.*

object Ex05FoldLeftTree extends zio.ZIOAppDefault:

  override def run =

    val tree = Tree(
      `🏘️`,
      Tree(🏡, Tree(🌳), Tree(🌴)),
      Tree(🏡, Tree(🌲), Tree(🌲))
    )

    val energy: TypedHomes => Int = {
      case 🌳 => 100
      case 🌴 => 5
      case 🌲 => 20
      case _  => 0
    }

    val result = tree.foldLeft[Int](0)((tree, acc) => energy(tree) + acc)

    zio.Console.printLine(s"$result eq. KWh")
