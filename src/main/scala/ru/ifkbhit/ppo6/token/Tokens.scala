package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.model.Operators.Operator
import ru.ifkbhit.ppo6.visit.Visitor

case class TokenMeta(startIndex: Int, length: Int)

sealed trait Token {
  def accept(visitor: Visitor[_]): Unit = {}

  def tokenMeta: TokenMeta
}

case class NumberToken(value: Double, override val tokenMeta: TokenMeta) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitNumber(this)
}

case class BracketToken(isOpen: Boolean, override val tokenMeta: TokenMeta) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitBracket(this)
}

case class OperatorToken(operator: Operator, override val tokenMeta: TokenMeta) extends Token {
  override def accept(visitor: Visitor[_]): Unit = visitor.visitOperator(this)
}

case class WhiteSpaceToken(override val tokenMeta: TokenMeta) extends Token

case object EofToken extends Token {
  override def tokenMeta: TokenMeta = TokenMeta(-1, 0)
}

object Token {

  implicit class TokenOps(token: Token) {
    def isOpenBracket: Boolean =
      token match {
        case BracketToken(isOpen, _) => isOpen
        case _ => false
      }

    def isOperator: Boolean =
      token match {
        case OperatorToken(_, _) => true
        case _ => false
      }

    def isWhitespace: Boolean =
      token match {
        case WhiteSpaceToken(_) => true
        case _ => false
      }
  }

}
