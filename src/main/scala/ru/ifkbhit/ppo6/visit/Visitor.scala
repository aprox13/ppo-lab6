package ru.ifkbhit.ppo6.visit

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken, Token}

trait Visitor[R] {
  def visitNumber(numberToken: NumberToken): Unit

  def visitOperator(operatorToken: OperatorToken): Unit

  def visitBracket(bracketToken: BracketToken): Unit

  def produce: R
}

trait AcceptAndProduceVisitor[R] {
  self: Visitor[R] =>

  def acceptAndProduce(tokens: Seq[Token]): R = {
    tokens.foreach(_.accept(this))

    produce
  }
}
