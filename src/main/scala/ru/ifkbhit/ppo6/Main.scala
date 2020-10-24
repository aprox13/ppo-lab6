package ru.ifkbhit.ppo6

import ru.ifkbhit.ppo6.token.{Token, Tokenizer, TokenizerImpl}
import ru.ifkbhit.ppo6.visit.{AcceptAndProduceVisitor, PrintVisitor, RPNVisitor, RpnCalculateVisitor, ToStringVisitor}
import util.Utils._

import scala.util.Try

object Main {

  def main(args: Array[String]): Unit =
    while (true) {
      println("\nInput:")
      val raw = scala.io.StdIn.readLine()
      calculate(raw)
    }

  private def calculate(raw: String): Unit = Try {
    println(s"Processing: '$raw'")

    val tokenizer = new TokenizerImpl
    tokenizer.accept(raw)

    val rpnVisitor = new RPNVisitor with AcceptAndProduceVisitor[Seq[Token]]
    val rpn = rpnVisitor.acceptAndProduce(tokenizer.tokens)
    println(s"Transform to reverse polish notation: ${tokensToString(rpn)}")

    val calculateVisitor = new RpnCalculateVisitor with AcceptAndProduceVisitor[Double]
    val res = calculateVisitor.acceptAndProduce(rpn).prettyString

    println(s"Result from RPN: $res")
  } recover {
    case t =>
      println(s"Couldn't calculate, because: ${t.getMessage}")
  }

  private def tokensToString(seq: Seq[Token]): String = {
    val toStringVisitor = new ToStringVisitor(spacesBetween = true) with AcceptAndProduceVisitor[String]

    toStringVisitor.acceptAndProduce(seq)
  }

}



