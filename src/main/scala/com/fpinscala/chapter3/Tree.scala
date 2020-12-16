package com.fpinscala.chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = {
    def loop[A](curr: Tree[A], acc: Int): Int = {
      curr match {
        case Leaf(_) => acc + 1
        case Branch(l: Tree[A], r: Tree[A]) => {
          loop(r, loop(l, acc + 1))
        }
      }
    }
    loop(t, 0)
  }
}
