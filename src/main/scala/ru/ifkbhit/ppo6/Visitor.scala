package ru.ifkbhit.ppo6

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken}

trait Visitor {
  def visitNumber(numberToken: NumberToken): Unit

  def visitOperator(operatorToken: OperatorToken): Unit

  def visitBracket(operatorToken: BracketToken): Unit
}

}