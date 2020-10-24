package ru.ifkbhit.ppo6.token

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


trait Tokenizer {
  def accept(source: String)

  def tokens: Seq[Token]
}

class TokenizerImpl extends Tokenizer {

  private val result: ArrayBuffer[Token] = ArrayBuffer()

  override def accept(source: String): Unit = acceptRec(source, 0)

  override def tokens: Seq[Token] = result.toSeq

  @tailrec
  private def acceptRec(source: String, index: Int): Unit = {
    val suitableOpt = TokenRule.rules
      .flatMap { rule =>
        rule.matches(source).map(Suitable(rule, _))
      }
      .groupBy(_.rule.weight)
      .maxByOption(_._1)

    require(
      suitableOpt.nonEmpty,
      s"Couldn't resolve token from first chars of string ${source.take(10)} [index: $index]"
    )
    val suitables = suitableOpt.get._2

    require(
      suitables.size == 1,
      s"Collision: ${suitables.map(s => s"${s.rule.name} with value '${s.value}'").mkString(", ")} [index: $index]"
    )

    val suitable = suitables.head
    result += suitable.produce(index)


    if (!result.lastOption.contains(EofToken)) {
      acceptRec(source.drop(suitable.getValueSize), index + suitable.getValueSize)
    }
  }

  private case class Suitable(rule: TokenRule, value: String) {
    def produce(index: Int): Token = {
      val res = rule.produce(value, index)
      require(res.nonEmpty, s"Couldn't produce token by rule ${rule.name} from '$value'")
      res.get
    }

    def getValueSize: Int = value.length
  }

}
