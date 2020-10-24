package ru.ifkbhit.ppo6.visit

import org.scalatest.{Matchers, WordSpec}
import ru.ifkbhit.ppo6.token.TokenizerImpl

class ToStringVisitorSpec extends WordSpec with Matchers {

  "ToStringVisitor" should {
    "correctly visit" in {
      val input = "   10 +\t\n10.2 - (1*1)"
      val expected = "10+10.2-(1*1)"

      val tokenizer = new TokenizerImpl
      tokenizer.accept(input)

      val visitor = new ToStringVisitor
      tokenizer.tokens.foreach(_.accept(visitor))

      visitor.produce shouldBe expected
    }
  }
}
