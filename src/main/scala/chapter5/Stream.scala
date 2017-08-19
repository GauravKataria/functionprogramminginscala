package scala.chapter5

import Stream._
trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Empty            => List() 
    case Cons(head, tail) => head() :: tail().toListRecursive
  }

  //tail recursive solution
  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty            => acc
      case Cons(head, tail) => go(tail(), head() :: acc)
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if(n > 0) Cons(h, () => t().take(n-1)) else Empty
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if(n == 0) this else if(n > 0) t().drop(n-1) else t() 
    case Empty => Empty 
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if(f(h())) Cons(h, () => t().takeWhile(f)) else Empty
    case Empty      => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean = 
    foldRight(true)((a, b) => f(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => 
      if(f(a)) Cons(() => a, () => b)
      else Empty
    )

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a,b) => Some(a))

  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if(f(h)) this else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Cons(() => h, () => t)) 

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a,b) => f(a) append b)

  // def mapViaUnfold[B](f: A => B): Stream[B]

  // def takeViaUnfold(n: Int): Stream[A]

  // def takeWhileViaUnfold(f: A => Boolean): Stream[A] 

  // def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C]

  // // special case of `zipWith`
  // def zip[B](s2: Stream[B]): Stream[(A,B)]

  // def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]

  // def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] 

  // def startsWith[A](s: Stream[A]): Boolean

  // def tails: Stream[Stream[A]]

  // def hasSubsequence[A](s: Stream[A]): Boolean

  // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B]

  // final def find(f: A => Boolean): Option[A]

}

case object Empty extends Stream[Nothing]
case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = 
      cons(f1, go(f1, f1+f0))

    go(0,1)  
  }

  // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] 

  // def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] 

  // def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] 

  // val fibsViaUnfold 

  // def fromViaUnfold(n: Int) 

  // def constantViaUnfold[A](a: A)

  // val onesViaUnfold 
}

