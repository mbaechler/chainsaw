package mb

import mb.Tree.{MapAcc, Path, map2, mapAccumulateHelp}

import java.util.Objects
import scala.annotation.tailrec

trait Tree[T]:

  def label: T

  def children: Seq[Tree[T]]

  def factory[U]: (U, List[Tree[U]]) => Tree[U]

  def mapLabel(f: T => T): Tree[T] = replaceLabel(f(label))

  def replaceLabel(newLabel: T): Tree[T] =
    Tree(label = newLabel, children = children*)

  def mapChildren(f: Seq[Tree[T]] => Seq[Tree[T]]): Tree[T] = replaceChildren(
    f(children)
  )

  def replaceChildren(newChildren: Seq[Tree[T]]): Tree[T] =
    Tree(label = label, children = newChildren*)

  def prependChild(child: Tree[T]): Tree[T] = mapChildren(child +: _)

  def appendChild(child: Tree[T]): Tree[T] = mapChildren(_ :+ child)

  def foldLeft[U](f: (T, U) => U, acc: U): U =
    Tree.foldlHelp(f, acc, List(this), List.empty)

  def foldRight[U](f: (T, U) => U, acc: U): U =
    foldLeft[List[T]]((t, l) => t :: l, List.empty[T])
      .foldLeft(acc)((acc, t) => f(t, acc))

  def count: Int = foldLeft((_, count) => count + 1, 0)

  def flatten: List[T] = foldRight(_ :: _, List.empty)

  def map[U](f: T => U): Tree[U] =
    mapAccumulate(f = (_, e) => () -> f(e), ())._2

  def filter(predicate: T => Boolean): Option[Tree[T]] =
    if predicate(label)
    then Some(Tree(label, children = children.flatMap(_.filter(predicate))*))
    else None

  def indexedMap[U](f: (Int, T) => U): Tree[U] = {
    val function: (Int, T) => (Int, U) = (idx, elem) => (idx + 1, f(idx, elem))
    mapAccumulate(f = function, state = 0)._2
  }

  def zipWithPath: Tree[(Path, T)] =
    def zipWithPath(tree: Tree[T], path: Path): Tree[(Path, T)] =
      Tree(
        label = path -> tree.label,
        children = tree.children.zipWithIndex.map((child, idx) =>
          zipWithPath(child, path.sub(idx))
        )*
      )
    zipWithPath(this, Path(0))

  def mapAccumulate[S, U](f: (S, T) => (S, U), state: S): (S, Tree[U]) =
    val (updatedState, updatedLabel) = f(state, label)
    mapAccumulateHelp(
      f,
      updatedState,
      MapAcc(todo = children.toList, label = updatedLabel)
    )

  override def equals(obj: Any): Boolean =
    obj match
      case other: Tree[?] =>
        this.label == other.label && this.children == other.children
      case _ => false

  override def hashCode(): Int = Objects.hash(label, children)

case class ConstTree[T](
    override val label: T,
    override val children: Seq[Tree[T]]
) extends Tree[T]:
  override def factory[U]: (U, List[Tree[U]]) => Tree[U] = Tree.factory

object Tree:

  def factory[T]: (T, List[Tree[T]]) => Tree[T] = ConstTree.apply

  case class Path private (elements: Seq[Int]):
    def sub(index: Int): Path = Path(elements :+ index)

  object Path:
    def apply(first: Int, others: Int*) = new Path(first +: others)

  private case class MapAcc[T, U](
      todo: List[Tree[T]],
      label: U,
      done: List[Tree[U]] = Nil
  )

  def apply[T](label: T, children: Tree[T]*): Tree[T] =
    new ConstTree(label, List(children*))

  @tailrec
  private def foldlHelp[T, U](
      f: (T, U) => U,
      acc: U,
      trees: List[Tree[T]],
      nextSets: List[Seq[Tree[T]]]
  ): U =
    trees match
      case Nil =>
        nextSets match
          case set :: sets => foldlHelp(f, acc, set.toList, sets)
          case Nil         => acc

      case tree :: rest if tree.children.isEmpty =>
        foldlHelp(f, f(tree.label, acc), rest, nextSets)

      case tree :: rest =>
        foldlHelp(f, f(tree.label, acc), tree.children.toList, rest :: nextSets)

  @tailrec
  private def mapAccumulateHelp[S, T, U](
      f: (S, T) => (S, U),
      state: S,
      acc: MapAcc[T, U],
      stack: Seq[MapAcc[T, U]] = Nil
  ): (S, Tree[U]) =
    acc.todo match
      case Nil =>
        val node = Tree(acc.label, acc.done.reverse*)
        stack match
          case Nil => (state, node)
          case top :: rest =>
            mapAccumulateHelp(f, state, top.copy(done = node :: top.done), rest)

      case tree :: rest if tree.children.isEmpty =>
        val (updatedState, updatedLabel) = f(state, tree.label)
        mapAccumulateHelp(
          f,
          updatedState,
          acc.copy(todo = rest, done = Tree(updatedLabel) :: acc.done),
          stack
        )

      case tree :: rest =>
        val (updatedState, updatedLabel) = f(state, tree.label)
        mapAccumulateHelp(
          f,
          updatedState,
          MapAcc(todo = tree.children.toList, label = updatedLabel),
          acc.copy(todo = rest) :: stack.toList
        )

  case class Map2Acc[T, U, V](
      todoL: Seq[Tree[T]],
      todoR: Seq[Tree[U]],
      done: List[Tree[V]],
      label: V
  )

  def map2[T, U, V](f: (T, U) => V, tl: Tree[T], tr: Tree[U]): Tree[V] =
    mapAccumulate2[Unit, T, U, V](
      f = (s, t, u) => (s, f(t, u)),
      state = (),
      tl = tl,
      tr = tr
    )._2

  def indexedMap2[T, U, V](
      f: (Int, T, U) => V,
      tl: Tree[T],
      tr: Tree[U]
  ): Tree[V] =
    mapAccumulate2[Int, T, U, V](
      f = (s, t, u) => (s + 1, f(s, t, u)),
      state = 0,
      tl = tl,
      tr = tr
    )._2

  def andMap[T, U](tl: Tree[T => U], tr: Tree[T]): Tree[U] =
    map2[T => U, T, U](_.apply(_), tl, tr)

  private def mapAccumulate2[S, T, U, V](
      f: (S, T, U) => (S, V),
      state: S,
      tl: Tree[T],
      tr: Tree[U]
  ): (S, Tree[V]) =
    val (s, z) = f(state, tl.label, tr.label)
    val acc =
      Map2Acc(
        todoL = tl.children,
        todoR = tr.children,
        done = Nil,
        label = z
      )
    mapAccumulate2Help(f, s, acc, Nil)

  @tailrec
  private def mapAccumulate2Help[S, T, U, V](
      f: (S, T, U) => (S, V),
      state: S,
      acc: Map2Acc[T, U, V],
      stack: List[Map2Acc[T, U, V]]
  ): (S, Tree[V]) =
    (acc.todoL, acc.todoR) match
      case (l, r) if l.isEmpty || r.isEmpty =>
        val node = Tree(acc.label, acc.done.reverse*)
        stack match
          case Nil => (state, node)
          case top :: rest =>
            mapAccumulate2Help(
              f,
              state,
              top.copy(done = node :: top.done),
              rest
            )

      case (l, r) =>
        val (state_, label_) = f(state, l.head.label, r.head.label)
        val newAcc = Map2Acc(
          todoL = l.head.children,
          todoR = r.head.children,
          done = Nil,
          label = label_
        )
        val newStack = acc.copy(todoL = l.tail, todoR = r.tail) :: stack
        mapAccumulate2Help(f, state_, newAcc, newStack)

class LazyTree[T](override val label: T, override val children: Seq[Tree[T]])
    extends Tree[T]:
  override def factory[U]: (U, List[Tree[U]]) => Tree[U] = ConstTree.apply

object LazyTree:
  def unfold[S, T](s: S)(f: S => (T, LazyList[S])): LazyTree[T] =
    val (label, seeds) = f(s)
    LazyTree(label, children = seeds.map(subSeed => unfold(subSeed)(f)))
