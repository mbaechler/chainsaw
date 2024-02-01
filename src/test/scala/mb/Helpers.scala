package mb

import zio.IO
import zio.Console.printLine

import java.io.IOException

@FunctionalInterface
trait Show[T]:
  def show(t: T): String

object Helpers:
  
  given Show[String] = identity

  object TreeHelpers:
    extension [T: Show](tree: Tree[T])
      def show(title: String): IO[IOException, Unit] =
        printLine(title) *> tree.show

      def show: IO[IOException, Unit] =
        printLine(show(tree, indent = 1))

      private def show(subtree: Tree[T], indent: Int): String =
        summon[Show[T]].show(subtree.label)
          ++ "\n"
          ++ subtree.children
            .map("\\--" ++ show(_, indent + 3))
            .map(_.indent(indent))
            .mkString("")

  object ListHelpers:
    extension [T: Show](list: List[T])
      def show(title: String): IO[IOException, Unit] =
        printLine(title) *> list.show

      def show: IO[IOException, Unit] =
        printLine(list.mkString("[", ", ", "]"))
