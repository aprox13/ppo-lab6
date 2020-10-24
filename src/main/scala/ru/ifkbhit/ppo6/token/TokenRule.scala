package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.model.Operators

import scala.util.matching.Regex

sealed trait TokenRule {
  def matches(s: String): Option[String]

  def name: String = this.getClass.getSimpleName

  def produce(s: String, index: Int): Option[Token]

  def weight: Int
}

sealed abstract class RegexpTokenRule(
  regex: Regex,
  produceF: (String, Int) => Option[Token] = (_, _) => None,
  override val weight: Int = 0
) extends TokenRule {
  override def matches(s: String): Option[String] =
    regex.findPrefixOf(s)

  override def produce(s: String, index: Int): Option[Token] =
    produceF(s, index)
}

case object NumberTokenRule extends RegexpTokenRule(
  "(-\\s*)?\\d+(\\.(\\d+)?)?".r,
  TokenRule.produceF(_.replaceAll("\\s*", "").toDouble, NumberToken),
  weight = 1 // more than operators
)

case object WhitespacesTokenRule extends RegexpTokenRule(
  "\\s+".r,
  (_, index) => Some(WhiteSpaceToken(index))
)

case object BracketTokenRule extends RegexpTokenRule(
  "[()]".r,
  TokenRule.produceF(_ == "(", BracketToken),
  weight = 2
)

case object OperatorTokenRule extends RegexpTokenRule(
  Operators.RegexAll,
  (s, index) => Operators.parseSafe(s).map(OperatorToken(_, index))
)

case object EofTokenRule extends TokenRule {
  override def matches(s: String): Option[String] =
    Some(s).filter(_.isEmpty)

  override def produce(s: String, index: Int): Option[Token] =
    Some(EofToken)

  override def weight: Int = -1
}

object TokenRule {
  def rules: Seq[TokenRule] = Seq(
    NumberTokenRule,
    WhitespacesTokenRule,
    OperatorTokenRule,
    BracketTokenRule,
    EofTokenRule
  )


  def produceF[T](mapper: String => T, creator: (T, Int) => Token)(s: String, index: Int): Option[Token] =
    Some(creator(mapper(s), index))
}
