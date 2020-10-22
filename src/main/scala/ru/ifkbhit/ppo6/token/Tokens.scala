package ru.ifkbhit.ppo6.token

import scala.util.matching.Regex

object Tokens extends Enumeration {

  val Number: Token = RegexpToken("\\d+".r, "{{number}}")
  val Whitespaces: Token = RegexpToken("\\s+".r, "{{whitespace}}")
  val Bracket: Token = RegexpToken("[()]".r, "{{bracket}}")
  val Operator: Token = RegexpToken("[+\\-*/]".r, "{{operator}}")
  val EOF: Token = RegexpToken("^$".r, "{{eof}}")

  def allTokens: Seq[Token] =
    values.collect {
      case x: Token => x
    }.toSeq

  sealed abstract class Token(name: String) extends super.Val(name) {
    def matches(s: String): Option[String]
  }

  case class RegexpToken(regexp: Regex, name: String = "") extends Token(name) {
    override def matches(s: String): Option[String] =
      regexp.findPrefixOf(s)
  }

}
