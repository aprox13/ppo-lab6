package ru.ifkbhit.ppo6.util

import ru.ifkbhit.ppo6.token.{BracketToken, EofToken, NumberToken, OperatorToken, Token, WhiteSpaceToken}

object Utils {

  def tokenHumanReadable(token: Token): String =
    token match {
      case NumberToken(value, _) =>
        DoublePretty.prettify(value)
      case bt@BracketToken(_, _) =>
        if (bt.isOpenBracket) {
          "("
        } else {
          ")"
        }
      case OperatorToken(operator, _) =>
        operator.pattern
      case WhiteSpaceToken(_) =>
        " "
      case EofToken =>
        ""
    }


  trait Prettify[A] {
    def prettify(a: A): String
  }

  implicit object TokenPrettify extends Prettify[Token] {
    override def prettify(a: Token): String = tokenHumanReadable(a)
  }

  implicit object DoublePretty extends Prettify[Double] {
    override def prettify(value: Double): String =
      if (value == value.toInt.toDouble) {
        value.toInt.toString
      } else {
        value.toString
      }
  }

  implicit class PrettyIterableOps[A](it: IterableOnce[A]) {
    def prettify(implicit prettify: Prettify[A]): String =
      it.iterator.map(prettify.prettify).mkString(", ")
  }

  implicit class PrettySingleOps[A](a: A) {
    def prettyString(implicit prettify: Prettify[A]): String =
      prettify.prettify(a)
  }

}
