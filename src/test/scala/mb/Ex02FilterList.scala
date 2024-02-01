package mb

import mb.Helpers.*
import mb.Helpers.ListHelpers.show
import mb.TypedHomes.*

object Ex02FilterList extends zio.ZIOAppDefault:

  override def run =

    val things = List(`🏘️`, 🏡, 🌳, 🌴, 🏡, 🌲, 🌲)

    val filteredTrees = things.filter {
      case 🌳 | 🌴 | 🌲 => false
      case `🏘️` | 🏡   => true
    }

    filteredTrees.show
