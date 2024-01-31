package mb

import mb.Zipper.Crumb

import scala.annotation.tailrec

object Zipper:
  
  case class Crumb[T](label: T, before: List[Tree[T]], after: List[Tree[T]])

  def fromTree[T](tree: Tree[T]): Zipper[T] =
    Zipper(
      focus = tree,
      before = Nil,
      after = Nil,
      crumbs = Nil
    )

  def fromForest[T](tree: Tree[T], siblings: List[Tree[T]]): Zipper[T] =
    Zipper(
      focus = tree,
      before = Nil,
      after = siblings,
      crumbs = Nil
    )

class Zipper[T](
    focus: Tree[T],
    before: List[Tree[T]],
    after: List[Tree[T]],
    crumbs: List[Crumb[T]]
):
  def toTree: Tree[T] = tree

  def toForest: (Tree[T], List[Tree[T]]) =
    val zipper = root
    (zipper.tree, zipper.siblingsAfterFocus)

  def tree: Tree[T] = focus

  def forward: Option[Zipper[T]] =
    firstChild.orElse(nextSibling).orElse(nextSiblingOfAncestor)
  def backward: Option[Zipper[T]] =
    previousSibling.map(_.lastDescendant).orElse(parent)

  def parent: Option[Zipper[T]] =
    crumbs match
      case Nil => None
      case crumb :: rest =>
        Some(
          Zipper(
            focus = Tree(crumb.label, (before ::: focus :: after).reverse*),
            before = crumb.before,
            after = crumb.after,
            crumbs = rest
          )
        )

  def firstChild: Option[Zipper[T]] =
    focus.children match
      case Nil => None
      case head :: tail =>
        Some(
          Zipper(
            focus = head,
            before = Nil,
            after = tail,
            crumbs = Crumb(
              label = focus.label,
              before = before,
              after = after
            ) :: crumbs
          )
        )

  def lastChild: Option[Zipper[T]] =
    focus.children match
      case Nil => None
      case head :: tail =>
        Some(
          Zipper(
            focus = head,
            before = tail,
            after = Nil,
            crumbs = Crumb(
              label = focus.label,
              before = before,
              after = after
            ) :: crumbs
          )
        )

  def root: Zipper[T] =
    parent.map(_.root).getOrElse(firstSibling)

  def firstSibling: Zipper[T] =
    previousSibling.map(_.firstSibling).getOrElse(this)

  def lastDescendant: Zipper[T] =
    lastChild.map(_.lastDescendant).getOrElse(this)

  def nextSibling: Option[Zipper[T]] =
    after match
      case Nil => None
      case next :: rest =>
        Some(
          Zipper(
            focus = next,
            before = focus :: before,
            after = rest,
            crumbs = crumbs
          )
        )

  def previousSibling: Option[Zipper[T]] =
    before match
      case Nil => None
      case previous :: rest =>
        Some(
          Zipper(
            focus = previous,
            before = rest,
            after = focus :: after,
            crumbs = crumbs
          )
        )

  def findNext(predicate: T => Boolean): Option[Zipper[T]] =
    LazyList.unfold(this)(s => s.forward.map(s -> _))
      .find(zipper => predicate(zipper.label))

  def findPrevious(predicate: T => Boolean): Option[Zipper[T]] =
    LazyList.unfold(this)(s => s.backward.map(s -> _))
      .find(zipper => predicate(zipper.label))
  
  def findFromRoot(predicate: T => Boolean): Option[Zipper[T]] =
    LazyList.unfold(root)(s => s.forward.map(s -> _))
      .find(zipper => predicate(zipper.label))

  def label: T = tree.label

  def children: List[Tree[T]] =
    tree.children

  def siblingsBeforeFocus: List[Tree[T]] =
    before.reverse

  def siblingsAfterFocus: List[Tree[T]] =
    after

  def updateTree(f: Tree[T] => Tree[T]): Zipper[T] =
    Zipper(focus = f(focus), before = before, after = after, crumbs = crumbs)

  def replaceTree(t: Tree[T]): Zipper[T] =
    Zipper(focus = t, before = before, after = after, crumbs = crumbs)

  def removeTree: Option[Zipper[T]] =
    (crumbs, before, after) match
      case (Nil, Nil, Nil) => None
      case (crumb :: rest, before, after) =>
        Some(
          Zipper(
            focus = Tree(crumb.label, before ::: after*),
            before = crumb.before,
            after = crumb.after,
            crumbs = rest
          )
        )
      case (Nil, b :: bs, after) =>
        Some(
          Zipper(
            focus = b,
            before = bs,
            after = after,
            crumbs = Nil
          )
        )
      case (Nil, Nil, a :: as_) =>
        Some(
          Zipper(
            focus = a,
            before = Nil,
            after = as_,
            crumbs = Nil
          )
        )
  def updateLabel(f: T => T): Zipper[T] =
    updateTree(_.mapLabel(f))

  def replaceLabel(t: T): Zipper[T] =
    updateLabel(_ => t)

  def prepend(t: Tree[T]): Zipper[T] =
    Zipper(focus = focus, before = t :: before, after = after, crumbs = crumbs)

  def append(t: Tree[T]): Zipper[T] =
    Zipper(focus = focus, before = before, after = t :: after, crumbs = crumbs)

  def nextSiblingOfAncestor: Option[Zipper[T]] =
    parent.flatMap(p => p.nextSibling.orElse(p.nextSiblingOfAncestor))
