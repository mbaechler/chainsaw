package mb

import zio.{UIO, ZIO}
import zio.test.*
import zio.test.Assertion.*

object TreeSpec extends ZIOSpecDefault:

  extension [T] (self: Tree[T])
    def debugTree: UIO[Tree[T]] =
      ZIO.debug(debugTree(self, 0)).as(self)
    private def debugTree(tree: Tree[T], indent: Int): String =
      val children =
        tree.children match
          case Nil => "[]"
          case l => l.map(t => debugTree(t, indent + 2)).mkString("\n[ ", "\n, ", "\n]").indent(indent + 2)
      s"""tree ${tree.label} $children"""

  def treeWithoutChild[R, T](labelGen: Gen[R, T]): Gen[R, Tree[T]] =
    labelGen.map(label => Tree(label))

  def treeWithChildren[R1, R2, T](
      labelGen: Gen[R1, T],
      childrenCountGen: Gen[R2, Int]
  ): Gen[R1 & R2, (Int, Tree[T])] =
    for
      childrenCount <- childrenCountGen
      tree <- treeWithNChildren(labelGen, childrenCount)
    yield childrenCount -> tree

  def treeWithNChildren[R, T](
      labelGen: Gen[R, T],
      count: Int
  ): Gen[R, Tree[T]] =
    for
      children <- Gen.listOfN(count)(treeWithoutChild(labelGen))
      label <- labelGen
    yield Tree(label, children = children)

  case class GeneratedTree[T](treeCount: Int, tree: Tree[T])

  def anyTree[R, T](
      labelGen: Gen[R, T]
  ): Gen[R, GeneratedTree[T]] =
    for
      label <- labelGen
      children <- Gen.weighted(
        Gen.const(Nil) -> 5,
        Gen.listOfBounded(0, 10)(anyTree(labelGen)) -> 1
      )
    yield GeneratedTree(
       1 + children.map(_.treeCount).sum,
      Tree(label, children.map(_.tree))
    )

  def counterGen: Gen[Any, Int] = Gen.fromIterable[Any, Int](LazyList.unfold(0)(i => Some(i, i + 1)))

  val spec = suite("Tree")(
    suite("count")(
      test("singleton count is 1") {
        assert(Tree(label = ()).count)(equalTo(1))
      },
      test("count of flat tree is children + 1") {
        check(
          treeWithChildren(
            labelGen = Gen.const(()),
            childrenCountGen = Gen.int(0, 100)
          )
        ) { (childrenCount, tree) =>
          assert(tree.count)(equalTo(childrenCount + 1))
        }
      },
      test("count of hardcoded tree") {
        val tree = Tree(
          "1",
          Tree("1.1"),
          Tree("1.2", Tree("1.2.1")),
          Tree("1.3", Tree("1.3.1", Tree("1.3.1.1"), Tree("1.3.1.2")))
        )
        assert(tree.count)(equalTo(8))
      }
    ),
    suite("foldLeftTest")(
      test("foldLeft hardcoded tree") {
        for
          tree <- Tree(1, Tree(2), Tree(3, Tree(4)), Tree(5, Tree(6, Tree(7), Tree(8)))).debugTree
        yield
          assert(
            tree
              .foldLeft[List[Int]]((label, acc) => acc.appended(label), Nil)
          )(equalTo(List(1, 2, 3, 4, 5, 6, 7, 8)))
      },
      test("foldLeft should accumulate every node") {
        check(anyTree(Gen.const(()))) { generated =>
          assert(generated.tree.foldLeft[Int]((_, acc) => acc + 1, 0))(
            equalTo(generated.treeCount)
          )
        }
      },
      test("foldLeft should traverse the tree depth-first") {
        check(anyTree(counterGen)) { generated =>
          for
            _ <- ZIO.debug(s"in $generated")
            tree <- generated.tree.debugTree
          yield assert(tree.foldLeft[List[Int]]((label, acc) => acc.appended(label), Nil))(
            equalTo(List.range(0, generated.treeCount))
          )
        }
      }@@ TestAspect.shrinks(0)
    )
  )
