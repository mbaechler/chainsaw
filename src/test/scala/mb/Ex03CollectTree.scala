package mb

import mb.Helpers.TreeHelpers.*
import mb.Helpers.{*, given}
import mb.TypedHomes.*

object Ex03CollectTree extends zio.ZIOAppDefault:

  override def run =

    val tree = Tree(
      "Neighbourhood",
      Tree("Neighbourhood", Tree("Oak"), Tree("PalmTree")),
      Tree("House", Tree("Oak"), Tree("PalmTree")),
      Tree("House", Tree("Evergreen"), Tree("Evergreen"), Tree("Olive Tree")),
      Tree("Building", Tree("Evergreen"), Tree("Evergreen"))
    )

    val parse: PartialFunction[String, TypedHomes] = {
      case "Oak"           => 🌳
      case "PalmTree"      => 🌴
      case "Evergreen"     => 🌲
      case "Neighbourhood" => `🏘️`
      case "House"         => 🏡
    }

    val collected = tree.collect(parse).get

    tree.show("original") *>
      collected.show("collected")
