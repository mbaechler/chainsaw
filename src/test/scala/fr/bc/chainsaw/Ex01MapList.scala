package fr.bc.chainsaw

import fr.bc.chainsaw.Helpers.ListHelpers.*

object MapList extends zio.ZIOAppDefault:

  enum TypedTree:
    case 🌳
    case 🌴
    case 🌲

  import TypedTree.*

  given Show[TypedTree] = {
    case 🌳 => "🌳"
    case 🌴 => "🌴"
    case 🌲 => "🌲"
  }

  val run =
    val parse: String => TypedTree = {
      case "Oak"       => 🌳
      case "PalmTree"  => 🌴
      case "Evergreen" => 🌲
    }
    val result = List("Oak", "PalmTree", "Evergreen").map(parse)
    result.show
