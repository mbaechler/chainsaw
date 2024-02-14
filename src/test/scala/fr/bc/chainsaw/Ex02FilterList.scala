package fr.bc.chainsaw

import fr.bc.chainsaw.Helpers.*
import fr.bc.chainsaw.Helpers.ListHelpers.show
import fr.bc.chainsaw.TypedHomes.*

object Ex02FilterList extends zio.ZIOAppDefault:

  override def run =

    val things = List(`🏘️`, 🏡, 🌳, 🌴, 🏡, 🌲, 🌲)

    val filteredTrees = things.filter {
      case 🌳 | 🌴 | 🌲 => false
      case `🏘️` | 🏡   => true
    }

    filteredTrees.show
