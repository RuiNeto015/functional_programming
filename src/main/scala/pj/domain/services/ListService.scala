package pj.domain.services

import pj.domain.Result

import scala.Ordered
import math.Ordered.orderingToOrdered

object ListService:
  def hasDuplicates[T](l: List[T]): Boolean =
    l.toSet.size != l.size

  def hasDuplicatedValue[T](list: List[T], selector: T => Any): Boolean =
    list.distinctBy(selector).size != list.size

  def sortList[T: Ordering](l: List[T]): List[T] =
    l.sortWith((s, t) => s.compare(t) < 0)

  def sequence[T](list: List[Result[T]]): Result[List[T]] =
    list.foldRight(Right(Nil): Result[List[T]]) { (result, acc) =>
      for {
        value <- result
        list <- acc
      } yield value :: list
    }