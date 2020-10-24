package ru.ifkbhit.ppo6.model

import scala.util.matching.Regex

object Operators extends Enumeration {

  val Plus: Operator = Operator("+", 1, _ + _)
  val Minus: Operator = Operator("-", 1, _ - _)
  val Multiply: Operator = Operator("*", 2, _ * _)
  val Divide: Operator = Operator("/", 2, _ / _)
  val RegexAll: Regex = "[+\\-*/]".r

  def parseSafe(s: String): Option[Operator] =
    Some(s).filter(_.nonEmpty)
      .flatMap(raw => operators.find(_.pattern == raw))

  def operators: Set[Operator] = Operators.values
    .collect {
      case x@Operator(_, _, _) => x
    }

  case class Operator(pattern: String, weight: Int, f: (Double, Double) => Double) extends super.Val(name = pattern) {
    def applyTo(a: Double, b: Double): Double = f(a, b)
  }

}
