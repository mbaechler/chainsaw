package fr.bc.chainsaw

import TypedHomes.*
import Helpers.{*, given}
import Helpers.TreeHelpers.*

object MapTree extends zio.ZIOAppDefault:

  override def run = {

    val parse: String => TypedHomes = {
      case "Oak"           => 🌳
      case "PalmTree"      => 🌴
      case "Evergreen"     => 🌲
      case "Neighbourhood" => `🏘️`
      case "House"         => 🏡
    }

    val tree = Tree(
      "Neighbourhood",
      Tree("House", Tree("Oak"), Tree("PalmTree")),
      Tree("House", Tree("Evergreen"), Tree("Evergreen"))
    )

    val result = tree.map(parse)

    tree.show("strings") *>
      result.show("types")
  }
