package mb

import mb.Helpers.ListHelpers.show
import mb.Helpers.*
import mb.TypedHomes.*

object Ex03CollectList extends zio.ZIOAppDefault:

  override def run =

    val parse: PartialFunction[String, TypedHomes] = {
      case "Oak"       => 🌳
      case "PalmTree"  => 🌴
      case "Evergreen" => 🌲
    }

    val result =
      List("Oak", "PalmTree", "Evergreen", "Olive Tree").collect(parse)

    result.show
