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

  private def resolveToken(suitable: List[Suitable], index: Int): Token =
    suitable match {
      case Nil =>
        throw new IllegalStateException(s"Couldn't resolve token from string at index: $index")
      case element :: Nil =>
        element.produce(index)
      case list =>
        val resolved = resolveConflictsRec(list.flatMap(_.produceSafe(index)))

        resolved.getOrElse(
          throw new IllegalStateException(s"Collision: ${suitable.map(s => s"${s.rule.name} with value '${s.value}'").mkString(", ")} [index: $index]")
        )
    }

  @tailrec
  private def acceptRec(source: String, index: Int): Unit = {
    val suitable = TokenRule.rules
      .flatMap { rule =>
        rule.matches(source).map(Suitable(rule, _))
      }.toList

    val token = resolveToken(suitable, index)


    result += token

    if (!token.isWhitespace) {
      lastNonWsToken = Some(token)
    }

    if (token != EofToken) {
      acceptRec(source.drop(token.tokenMeta.length), index + token.tokenMeta.length)
    }
  }

  private case class Suitable(rule: TokenRule, value: String) {
    def produce(index: Int): Token = {
      val res = produceSafe(index)
      require(res.nonEmpty, s"Couldn't produce token by rule ${rule.name} from '$value'")
      res.get
    }

    def produceSafe(index: Int): Option[Token] =
      rule.produce(value, index)
  }

}
