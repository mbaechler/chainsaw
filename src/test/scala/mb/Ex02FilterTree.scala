package mb

import TypedHomes.*
import Helpers.*
import Helpers.TreeHelpers.*

object Ex02FilterTree extends zio.ZIOAppDefault:

  override def run =

    val tree = Tree(
      `🏘️`,
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

    tree.show("original") *>
      filteredTrees.show("without trees") *>
      filteredHomes.show("without homes")
