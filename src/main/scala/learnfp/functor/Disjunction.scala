package learnfp.functor

object Disjunction {
  sealed trait Disjunction[+L, +R]
  case class LeftDisjunction[L, R](leftValue:L) extends Disjunction[L, R]
  case class RightDisjunction[L, R](rightValue:R) extends Disjunction[L, R]

  def left[L, R](lv:L):Disjunction[L, R] = LeftDisjunction[L, R](lv)
  def right[L, R](rv:R):Disjunction[L, R] = RightDisjunction[L, R](rv)
}

object DisjunctionInstance {
  import Disjunction._
  implicit def eitherInstance[L] = new Functor[({type E[A] = Disjunction[L, A]})#E] {
    override def fmap[A, B](a: Disjunction[L, A])(fx: A => B): Disjunction[L, B] = a match {
      case LeftDisjunction(lv) => left[L,B](lv)
      case RightDisjunction(rv) => right[L,B](fx(rv))
    }
  }

  implicit def leftInstance[L] = new Functor[({type E[A] = LeftDisjunction[L, A]})#E] {
    override def fmap[A, B](a: LeftDisjunction[L, A])(fx: A => B): LeftDisjunction[L, B] = LeftDisjunction[L, B](a.leftValue)
  }

  implicit def rightInstance[L] = new Functor[({type E[A] = RightDisjunction[L, A]})#E] {
    override def fmap[A, B](a: RightDisjunction[L, A])(fx: A => B): RightDisjunction[L, B] = RightDisjunction(fx(a.rightValue))
  }

  implicit def baseToFunctorOps[L, R, D[L, R] <: Disjunction[L, R]](disjunction: D[L, R])
                                 (implicit functor:Functor[({type E[A] = D[L, A]})#E])= {
    new FunctorOps[R, ({type E[A] = D[L, A]})#E](disjunction)
  }

  class DisjunctionFxOps[A, R](fx:A => R) {
    def `<$>`[L](a:Disjunction[L, A]):Disjunction[L, R] = a fmap fx
  }

  implicit def toFxOps[A, R](fx:A => R):DisjunctionFxOps[A, R] = new DisjunctionFxOps(fx)
}

