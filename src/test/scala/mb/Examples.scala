package mb

import mb.TreeStringsToTypes.TypedHomes.`🏘️`
import zio.{ZIO, ZIOAppDefault}

trait Show[T]:
  def show(t: T): String
def show[T: Show](tree: Tree[T], indent: Int = 1): String =
  summon[Show[T]].show(tree.label)
    ++ "\n"
    ++ tree.children
      .map("\\--" ++ show(_, indent + 3))
      .map(_.indent(indent))
      .mkString("")








