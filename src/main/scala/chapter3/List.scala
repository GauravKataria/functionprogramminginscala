package scala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, tail) => 0
    case Cons(head, tail) => head * product(tail)   
  }

  def apply[A](list: A*): List[A] = {
    if(list.length == 0 ) Nil
    else Cons(list.head, apply(list.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(head, tail) => Cons(h, tail) 
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>  if(n == 0) l else drop(tail, n-1)
  }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>  if(f(head)) dropWhile(tail, f) else l 
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _)

  //foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))   
  //(Ans: we will get back original list)

  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum3(l: List[Int]): Int = 
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = 
    foldLeft(l, 1)(_ * _)

  def length3[A](l: List[A]): Int = 
    foldLeft(l, 0)((acc, _) =>  acc + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(reverse(as), z)((a,b) => f(b,a))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())((h, acc) => append(acc, h))

  def addOne(l: List[Int]): List[Int] = 
    foldLeft(l, List[Int]())((acc, h) => Cons(h+1, acc))

  def doubleToString(l: List[Int]): List[String] = 
    foldLeft(l, List[String]())((acc, h) => Cons(h.toString, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] = 
    foldLeft(as, List[B]())((acc, h) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldLeft(as, List[A]())((acc, h) => if(f(h)) Cons(h, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =  //using flatMap
    flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)

  def addTwoList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case(_, Nil) => Nil
    case(Nil, _) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoList(t1, t2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1, l2) match {
    case(_, Nil) => Nil
    case(Nil, _) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}

