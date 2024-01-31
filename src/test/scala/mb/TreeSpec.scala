package mb

import mb.Tree.Path
import zio.{UIO, ZIO}
import zio.test.*
import zio.test.Assertion.*

object TreeSpec extends ZIOSpecDefault:

  extension [T](self: Tree[T])
    def debugTree: UIO[Tree[T]] =
      ZIO.debug(debugTree(self, 0)).as(self)
    private def debugTree(tree: Tree[T], indent: Int): String =
      val children =
        tree.children match
          case Nil => "[]"
          case l =>
            l.map(t => debugTree(t, indent + 2))
              .mkString("\n[ ", "\n, ", "\n]")
              .indent(indent + 2)
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
    yield Tree(label, children = children*)

  case class GeneratedTree[T](treeCount: Int, tree: Tree[T])

  def anyTree[R, T](
      labelGen: Gen[R, T] = Gen.const(())
  ): Gen[R, GeneratedTree[T]] =
    for
      label <- labelGen
      children <- Gen.weighted(
        Gen.const(Nil) -> 5,
        Gen.listOfBounded(0, 10)(anyTree(labelGen)) -> 1
      )
    yield GeneratedTree(
      1 + children.map(_.treeCount).sum,
      Tree(label, children.map(_.tree)*)
    )

  def counterGen: Gen[Any, Int] =
    Gen.fromIterable[Any, Int](LazyList.unfold(0)(i => Some(i, i + 1)))

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
        for tree <- Tree(
            1,
            Tree(2),
            Tree(3, Tree(4)),
            Tree(5, Tree(6, Tree(7), Tree(8)))
          ).debugTree
        yield assert(
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
            _ <- ZIO.debug(s"in ${generated.tree}")
            tree <- generated.tree.debugTree
          yield assert(
            tree.foldLeft[List[Int]]((label, acc) => acc.appended(label), Nil)
          )(
            equalTo(List.range(0, generated.treeCount))
          )
        }
      } @@ TestAspect.shrinks(0) @@ TestAspect.ignore
    ),
    suite("filter")(
      test("filter with always false on single node should return None") {
        val actual = Tree("a").filter(_ => false)
        assert(actual)(isNone)
      },
      test("filter with always true on single node should return same tree") {
        val input = Tree("a")
        val actual = input.filter(_ => true)
        assert(actual)(isSome(equalTo(input)))
      },
      test("filter returning false for second level should return the root") {
        val input = Tree("a", Tree("b"))
        val actual = input.filter(label => label == "a")
        assert(actual)(isSome(equalTo(Tree("a"))))
      },
      test(
        "filter cutting after second level should not be more than 2 level deep"
      ) {
        check(anyTree()) { input =>
          val actual = input.tree.zipWithPath
            .filter((path, _) => path.elements.sizeIs <= 3)
            .get
          assert(actual.flatten)(
            not(
              exists(hasField("path", _._1.elements, hasSize(isGreaterThan(3))))
            )
          )
        }
      }
    ),
    suite("zipWithPath")(
      test("should zip singleton with [0]") {
        val actual = Tree("a").zipWithPath
        assert(actual)(equalTo(Tree(Path(0) -> "a")))
      },
      test("should zip second level with [0, 0]") {
        val actual = Tree("a", Tree("b")).zipWithPath
        assert(actual)(equalTo(Tree(Path(0) -> "a", Tree(Path(0, 0) -> "b"))))
      },
      test("should zip second level with contiguous indices") {
        check(treeWithNChildren(Gen.const("a"), 10)) { tree =>
          assert(tree.zipWithPath.children.map(_.label._1))(
            equalTo(List.range(0, 10).map(i => Path(0, i)))
          )
        }
      }
    ),
    suite("unfold")(
      test("singleton") {
        assert(LazyTree.unfold(Path(0))(path => path -> LazyList.empty))(
          equalTo(Tree(Path(0)))
        )
      },
      test("single level tree") {
        assert(
          LazyTree.unfold(Path(0))(path =>
            path -> (if path.elements.sizeIs < 2 then
                       LazyList.range(0, 5).map(i => path.sub(i))
                     else LazyList.empty)
          )
        )(
          equalTo(
            Tree(
              Path(0),
              Tree(Path(0, 0)),
              Tree(Path(0, 1)),
              Tree(Path(0, 2)),
              Tree(Path(0, 3)),
              Tree(Path(0, 4))
            )
          )
        )
      },
      test("unfold should be able to replicate zipWithPath feature") {
        check(anyTree()) { input =>
          val unfolded = LazyTree.unfold(Path(0))(path =>
            path -> LazyList.iterate(0)(_ + 1).map(idx => path.sub(idx))
          )
          val zipped = input.tree.zipWithPath.map(_._1)
          assert(
            Tree.map2((p1: Path, p2: Path) => p1 == p2, unfolded, zipped).flatten
          )(not(contains(false)))
        }
      },
      test("unfold should generate a path tree") {
        val input = Tree((), Tree((), Tree(())), Tree(()))
        val unfolded = LazyTree.unfold(Path(0))(path =>
          path -> LazyList.iterate(0)(_ + 1).map(idx => path.sub(idx))
        )
        assert(Tree.map2((p1: Path, p2: Unit) => p1, unfolded, input))(
          equalTo(
            Tree(
              Path(0),
              Tree(Path(0, 0), Tree(Path(0, 0, 0))),
              Tree(Path(0, 1))
            )
          )
        )
      }
    )
  )
