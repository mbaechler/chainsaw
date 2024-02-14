package fr.bc.chainsaw

import cats.syntax.traverse.*
import fr.bc.chainsaw.Helpers.*
import fr.bc.chainsaw.Helpers.ListHelpers.show
import fr.bc.chainsaw.TypedHomes.*
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
