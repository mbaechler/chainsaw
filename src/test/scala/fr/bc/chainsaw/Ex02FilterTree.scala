package fr.bc.chainsaw

import fr.bc.chainsaw.Helpers.*
import fr.bc.chainsaw.Helpers.TreeHelpers.*
import fr.bc.chainsaw.TypedHomes.*

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
