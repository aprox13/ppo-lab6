package ru.ifkbhit.ppo6.util

import ru.ifkbhit.ppo6.token.{BracketToken, EofToken, NumberToken, OperatorToken, Token, WhiteSpaceToken}

object Utils {

  def tokenHumanReadable(token: Token): String =
    token match {
      case NumberToken(value, _) =>
        if (value == value.toInt.toDouble) {
          value.toInt.toString
        } else {
          value.toString
        }
      case bt@BracketToken(_, _) =>
        if (bt.isOpenBracket) {
          "("
        } else {
          ")"
        }
      case OperatorToken(operator, _) =>
        operator.pattern
      case WhiteSpaceToken(tokenMeta) =>
        " "
      case EofToken =>
        ""
    }
}
