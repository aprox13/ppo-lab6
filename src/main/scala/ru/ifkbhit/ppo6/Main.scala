package ru.ifkbhit.ppo6

import ru.ifkbhit.ppo6.token.{Tokenizer, TokenizerImpl}
import ru.ifkbhit.ppo6.visit.PrintVisitor

object Main {
  def main(args: Array[String]): Unit = {
    val s = "1000+\t\n10.2"
    val tokenizer = new TokenizerImpl
    tokenizer.accept(s)

    tokenizer.tokens.foreach(_.accept(PrintVisitor))
  }
}
