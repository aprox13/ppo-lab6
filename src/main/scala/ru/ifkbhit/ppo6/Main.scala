package ru.ifkbhit.ppo6

import ru.ifkbhit.ppo6.token.{Tokenizer, TokenizerImpl}
import ru.ifkbhit.ppo6.visit.{PrintVisitor, RPNVisitor}

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {

    val input = "1 + ( 2 - 3 ) * 4"

    val tokenizer = new TokenizerImpl
    tokenizer.accept(input)

    val rpnVisitor = new RPNVisitor

    tokenizer.tokens.foreach(_.accept(rpnVisitor))

    rpnVisitor.produce.foreach(_.accept(PrintVisitor))

  }
}



