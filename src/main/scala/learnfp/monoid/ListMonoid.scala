package learnfp.monoid

object ListMonoid {
  implicit def listMonoid[T]:Monoid[List[T]] = new Monoid[List[T]] {
    override def mzero: List[T] = List.empty
    override def mappend(lhs: List[T], rhs: List[T]): List[T] = lhs ++ rhs
  }
}
