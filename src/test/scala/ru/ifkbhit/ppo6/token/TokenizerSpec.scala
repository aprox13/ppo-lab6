package ru.ifkbhit.ppo6.token

import org.scalatest.{Assertion, Matchers, WordSpec}
import ru.ifkbhit.ppo6.token.Tokens.Token

class TokenizerSpec extends WordSpec with Matchers {

  private case class TestCase(source: String, expected: Seq[Token]) {
    def check(): Assertion =
      new Tokenizer(source).tokens
        .toSeq.map(_.token) should contain theSameElementsInOrderAs expected
  }

  "Tokenizer" should {
    "correctly parse single tokens" when {
      "comes nothing" in {
        TestCase("", Seq()).check()
      }

      "comes positive number" in {
        TestCase("10", Seq(Tokens.Number)).check()
      }

      "comes bracket" in {
        Seq("(", ")").foreach(TestCase(_, Seq(Tokens.Bracket)).check())
      }

      "comes whitespaces" in {
        Seq(" ", "   ", "\t", System.lineSeparator()).foreach(TestCase(_, Seq(Tokens.Whitespaces)).check())
      }

      "comes operators" in {
        Seq("+", "-", "/", "*").foreach(TestCase(_, Seq(Tokens.Operator)).check())
      }
    }

    "correctly parse multiple tokens" in {

      TestCase(
        " 10+    20\t*(3/ \n0)",
        Seq(
          Tokens.Whitespaces, Tokens.Number, Tokens.Operator, Tokens.Whitespaces, Tokens.Number, Tokens.Whitespaces,
          Tokens.Operator, Tokens.Bracket, Tokens.Number, Tokens.Operator, Tokens.Whitespaces, Tokens.Number, Tokens.Bracket
        )
      ).check()
    }
  }

}
