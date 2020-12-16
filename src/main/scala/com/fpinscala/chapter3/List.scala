package com.fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => t
  }
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons(a, t)
  }
  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if(f(h)) => dropWhile(t, f)
    case _ => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }
  def foldRightLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((z,a) => f(a, z))

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }
  def foldLeftRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, z) => f(z, a))


  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)((a,z) => {
    if(a == 0) 0
    else a*z
  })

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Double]): Double = ints match {
    case Nil => 1.0
    case Cons(0.0, xs) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def length[A](as: List[A]):Int = foldRight(as, 0)((_, z) => z + 1)

  def sumLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productLeft(ds: List[Double]): Double = foldLeft(ds, 0.0)(_ * _)
  def lengthLeft(as: List[String]): Int = foldLeft(as, 0)((z, _) => z + 1)
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((z, a) => Cons(a, z))
  def append[A](a: A, as: List[A]): List[A] = foldRight(as, List(a))((a, z) => Cons(a, z))

  def flatten[A](aas: List[List[A]]): List[A] = foldRight(aas, Nil: List[A])((as, z) => foldRight(as, z)(Cons(_, _)))
  def addOne(ints: List[Int]): List[Int] = foldRight(ints, Nil: List[Int])((i, z) => Cons(i+1, z))
  def dToString(ds: List[Double]): List[String] = foldRight(ds, Nil: List[String])((d, z) => Cons(d.toString, z))
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, z) => Cons(f(a), z))
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, z) => {
    if(f(a))Cons(a,z)
    else z
  })
  def filterOdds(as: List[Int]): List[Int] = filter(as)(_ % 2 == 0)
  def flatMap[A, B](aas: List[A])(f: A => List[B]): List[B] = foldRight(aas, Nil: List[B])((a, bs) => foldRight(f(a), bs)(Cons(_,_)))

  def filterFlatMap[A](as: List[A])(f: A => Boolean):List[A] = flatMap(as)(a => if(f(a)) Cons(a, Nil: List[A]) else Nil)
  def zipInts(is1: List[Int], is2: List[Int]): List[Int] = {
    def loop(i1: List[Int], i2: List[Int], acc: List[Int]): List[Int] = {
      (i1, i2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1,t1), Cons(h2,t2))=> loop(t1, t2, Cons(h1+h2, acc))
      }
    }
    reverse(loop(is1, is2, Nil))
  }
  def zipWith[A](aas1: List[A], aas2: List[A])(f: (A, A) => A): List[A] = {
    def loop(as1: List[A], as2: List[A], acc: List[A]): List[A] = {
      (as1, as2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
      }
    }
    reverse(loop(aas1, aas2, Nil))
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def buildSubsequence(as: List[A], acc: List[A], rem: Int): List[A] = {
      if(rem == 0) reverse(acc)
      else as match {
        case Nil => reverse(acc)
        case Cons(h, t) => buildSubsequence(t, Cons(h, acc), rem - 1)
      }
    }
    def loop(as: List[A], subLen: Int): Boolean = {
      as match {
        case Nil => false
        case Cons(_, t) => {
          val subSeq = buildSubsequence(as, Nil, subLen)
          if(subSeq == sub) true
          else loop(t, subLen)
        }
      }
    }

    val subLen = length(sub)
    if(subLen == 0) false
    else loop(sup, length(sub))

  }
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
