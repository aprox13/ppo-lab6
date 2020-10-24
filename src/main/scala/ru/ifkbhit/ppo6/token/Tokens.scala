package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.model.Operators.Operator
import ru.ifkbhit.ppo6.visit.Visitor

sealed trait Token {
  def accept(visitor: Visitor[_]): Unit = {}

  def startIndex: Int
}

case class NumberToken(value: Double, override val startIndex: Int) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitNumber(this)
}

case class BracketToken(isOpen: Boolean, override val startIndex: Int) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitBracket(this)
}

case class OperatorToken(operator: Operator, override val startIndex: Int) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitOperator(this)
}

case class WhiteSpaceToken(override val startIndex: Int) extends Token

case object EofToken extends Token {
  override def startIndex: Int = -1
}

