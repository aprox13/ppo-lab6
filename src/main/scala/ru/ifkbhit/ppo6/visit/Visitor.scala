package ru.ifkbhit.ppo6.visit

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken}

trait Visitor[R] {
  def visitNumber(numberToken: NumberToken): Unit

  def visitOperator(operatorToken: OperatorToken): Unit

  def visitBracket(bracketToken: BracketToken): Unit

  def produce: R
}
