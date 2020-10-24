package ru.ifkbhit.ppo6.model

import scala.util.matching.Regex

object Operators extends Enumeration {

  val Plus: Operator = Operator("+", 1)
  val Minus: Operator = Operator("-", 1)
  val Multiply: Operator = Operator("*", 2)
  val Divide: Operator = Operator("/", 2)
  val RegexAll: Regex = "[+\\-*/]".r

  def parseSafe(s: String): Option[Operator] =
    Some(s).filter(_.nonEmpty)
      .flatMap(raw => operators.find(_.pattern == raw))

  def operators: Set[Operator] = Operators.values
    .collect {
      case x@Operator(_, _) => x
    }

  case class Operator(pattern: String, weight: Int) extends super.Val(name = pattern)

}
