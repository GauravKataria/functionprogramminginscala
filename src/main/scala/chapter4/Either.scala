package scala.chapter4

import scala.{Option => _, Either => _, _}

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match { 
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    this flatMap { av => b match {
        case Right(bv) => Right(f(av, bv))
        case Left(e) => Left(e)
      }
    }

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for{ av <- this; bv <- b } yield f(av, bv)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight[Either[E,List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_ :: _))
}