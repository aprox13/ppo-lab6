package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.model.Operators

import scala.util.matching.Regex

sealed trait TokenRule {
  def matches(s: String): Option[String]

  def name: String = this.getClass.getSimpleName

  def produce(s: String, index: Int): Option[Token]
}

sealed abstract class RegexpTokenRule(
  regex: Regex,
  produceF: (String, Int) => Option[Token] = (_, _) => None,
) extends TokenRule {
  override def matches(s: String): Option[String] =
    regex.findPrefixOf(s)

  override def produce(s: String, index: Int): Option[Token] =
    produceF(s, index)
}

case object NumberTokenRule extends RegexpTokenRule(
  "(-\\s*)?\\d+(\\.(\\d+)?)?".r,
  TokenRule.produceF(_.replaceAll("\\s*", "").toDouble, NumberToken),
)

case object WhitespacesTokenRule extends RegexpTokenRule(
  "\\s+".r,
  (s, index) => Some(WhiteSpaceToken(TokenMeta(index, s.length)))
)

case object BracketTokenRule extends RegexpTokenRule(
  "[()]".r,
  TokenRule.produceF(_ == "(", BracketToken)
)

case object OperatorTokenRule extends RegexpTokenRule(
  Operators.RegexAll,
  (s, index) => Operators.parseSafe(s).map(OperatorToken(_, TokenMeta(index, s.length)))
)

case object EofTokenRule extends TokenRule {
  override def matches(s: String): Option[String] =
    Some(s).filter(_.isEmpty)

  override def produce(s: String, index: Int): Option[Token] =
    Some(EofToken)
}

object TokenRule {
  def rules: Seq[TokenRule] = Seq(
    NumberTokenRule,
    WhitespacesTokenRule,
    OperatorTokenRule,
    BracketTokenRule,
    EofTokenRule
  )


  def produceF[T](mapper: String => T, creator: (T, TokenMeta) => Token)(s: String, index: Int): Option[Token] =
    Some(creator(mapper(s), TokenMeta(index, s.length)))
}
