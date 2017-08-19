package scala.chapter3

import scala.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
  	case Leaf(a) => 1
  	case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
  	case Leaf(a: Int) => a
  	case Branch(l, r) => max(maximum(l), maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
  	case Leaf(a) => 0
  	case Branch(l, r) => 1 + max(depth(l), depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  	case Leaf(a) => Leaf(f(a))
  	case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  	case Leaf(a) => f(a)
  	case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = 
  	fold(t)(_ => 1)(_ + _)

  def maximum2(t: Tree[Int]): Int =
  	fold(t)(identity)(max(_,_))

  def depth2[A](t: Tree[A]): Int = 
  	fold(t)(_ => 1)(1 + max(_,_))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = 
  	fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}