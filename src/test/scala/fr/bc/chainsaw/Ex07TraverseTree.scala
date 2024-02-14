package fr.bc.chainsaw

import cats.syntax.traverse.*
import fr.bc.chainsaw.Helpers.*
import fr.bc.chainsaw.Helpers.TreeHelpers.show
import fr.bc.chainsaw.TypedHomes.*
import zio.interop.catz.core.*

object Ex07TraverseTree extends zio.ZIOAppDefault:

  override def run =

    val parse: String => Either[String, TypedHomes] = {
      case "Oak"           => Right(🌳)
      case "PalmTree"      => Right(🌴)
      case "Evergreen"     => Right(🌲)
      case "Neighbourhood" => Right(`🏘️`)
      case "House"         => Right(🏡)
      case s               => Left(s"parse error for $s")
    }

    val valid = Tree(
      "Neighbourhood",
      Tree("House", Tree("Oak"), Tree("PalmTree")),
      Tree("House", Tree("Evergreen"), Tree("Evergreen"))
    )

    val invalid = Tree(
      "Neighbourhood",
      Tree("House", Tree("Oak"), Tree("PalmTree")),
      Tree("House", Tree("Evergreen"), Tree("Evergreen"), Tree("Olive Tree"))
    )

    val left = invalid.traverse(parse)
    val right = valid.traverse(parse)

    zio.Console.printLine(left) *>
      right.traverse(_.show("right"))
