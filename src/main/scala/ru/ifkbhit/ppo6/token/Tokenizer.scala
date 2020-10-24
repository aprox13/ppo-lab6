package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.model.Operators

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import ru.ifkbhit.ppo6.token.Token.TokenOps


trait Tokenizer {
  def accept(source: String)

  def tokens: Seq[Token]
}

class TokenizerImpl extends Tokenizer {

  private val result: ArrayBuffer[Token] = ArrayBuffer()
  private var lastNonWsToken: Option[Token] = None

  private def highlight(s: String, index: Int): String =
    s + System.lineSeparator() + (" " * index + "^") + System.lineSeparator()

  override def accept(source: String): Unit = acceptRec(source, 0)

  override def tokens: Seq[Token] = result.toSeq

  private def resolveConflict(a: Token, b: Token): Option[Token] =
    (a, b) match {
      case (numToken@NumberToken(_, _), opToken@OperatorToken(Operators.Minus, _)) =>

        if (result.isEmpty || lastNonWsToken.exists(_.isOpenBracket) || lastNonWsToken.exists(_.isOperator)) {
          Some(numToken)
        } else {
          Some(opToken)
        }
      case (_, _) =>
        None
    }

  @tailrec
  private def resolveConflictsRec(toResolve: List[Token]): Option[Token] = {
    toResolve match {
      case Nil =>
        None
      case single :: Nil =>
        Some(single)
      case list =>
        val reduced = list.combinations(2).flatMap {
          case a :: b :: Nil => resolveConflict(a, b).orElse(resolveConflict(b, a))
        }.toList

        resolveConflictsRec(reduced)
    }
  }

  private def resolveToken(suitable: List[Suitable], index: Int, source: String): Token =
    suitable match {
      case Nil =>
        throw new IllegalStateException(
          s"Couldn't resolve token from string at index: $index" +
            System.lineSeparator() +
            highlight(source, index)
        )
      case element :: Nil =>
        import element._
        element.produce(index)
          .getOrElse(
            throw new IllegalStateException(
              s"Couldn't produce token by rule ${rule.name} from '$value'" +
                System.lineSeparator() +
                highlight(source, index)
            )
          )
      case list =>
        val resolved = resolveConflictsRec(list.flatMap(_.produce(index)))

        resolved.getOrElse(
          throw new IllegalStateException(
            s"Collision: ${suitable.map(s => s"${s.rule.name} with value '${s.value}'").mkString(", ")} at index: $index" +
              System.lineSeparator() +
              highlight(source, index)
          )
        )
    }

  @tailrec
  private def acceptRec(s: String, index: Int): Unit = {
    val source = s.drop(index)
    val suitable = TokenRule.rules
      .flatMap { rule =>
        rule.matches(source).map(Suitable(rule, _))
      }.toList

    val token = resolveToken(suitable, index, s)

    result += token

    if (!token.isWhitespace) {
      lastNonWsToken = Some(token)
    }

    if (token != EofToken) {
      acceptRec(s, index + token.tokenMeta.length)
    }
  }

  private case class Suitable(rule: TokenRule, value: String) {
    def produce(index: Int): Option[Token] =
      rule.produce(value, index)
  }

}
