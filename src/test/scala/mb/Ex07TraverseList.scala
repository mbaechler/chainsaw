package mb

import mb.Helpers.ListHelpers.show
import mb.Helpers.*
import mb.TypedHomes.*
import cats.syntax.traverse.*
import zio.interop.catz.core.*

object Ex07TraverseList extends zio.ZIOAppDefault:

  override def run =

    val parse: String => Either[String, TypedHomes] = {
      case "Oak"       => Right(🌳)
      case "PalmTree"  => Right(🌴)
      case "Evergreen" => Right(🌲)
      case s           => Left(s"$s is not a tree")
    }

    val left =
      List("Oak", "PalmTree", "Evergreen", "Olive Tree").traverse(parse)
    val right = List("Oak", "PalmTree", "Evergreen").traverse(parse)

    zio.Console.printLine(left) *>
      right.traverse(_.show("right"))
