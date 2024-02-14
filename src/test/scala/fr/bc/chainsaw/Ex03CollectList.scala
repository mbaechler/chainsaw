package fr.bc.chainsaw

import fr.bc.chainsaw.Helpers.*
import fr.bc.chainsaw.Helpers.ListHelpers.show
import fr.bc.chainsaw.TypedHomes.*

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
