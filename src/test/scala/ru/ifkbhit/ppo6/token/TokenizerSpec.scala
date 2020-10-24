package ru.ifkbhit.ppo6.token

import org.scalatest.{Assertion, Matchers, WordSpec}
import ru.ifkbhit.ppo6.model.Operators

class TokenizerSpec extends WordSpec with Matchers {

  private def expectedFailOn(source: String) = {
    val tokenizer = new TokenizerImpl

    an[IllegalStateException] should be thrownBy tokenizer.accept(source)
  }

  private case class TestCase(source: String, expected: Seq[Token]) {
    def check(): Assertion = {
      val tokenizer = new TokenizerImpl
      tokenizer.accept(source)

      tokenizer.tokens should contain theSameElementsInOrderAs expected
    }
  }

  private def meta(index: Int): TokenMeta = TokenMeta(index, 1)

  "Tokenizer" should {
    "correctly parse single tokens" when {
      "comes nothing" in {
        TestCase("", Seq(EofToken)).check()
      }

      "comes positive number int" in {
        TestCase("10", Seq(NumberToken(10.0, TokenMeta(0, 2)), EofToken)).check()
      }

      "comes negative number int" in {
        TestCase("-10", Seq(NumberToken(-10.0, TokenMeta(0, 3)), EofToken)).check()
      }

      "comes positive number double" in {
        TestCase("10.3", Seq(NumberToken(10.3, TokenMeta(0, 4)), EofToken)).check()
      }

      "comes negative number double" in {
        TestCase("-10.3", Seq(NumberToken(-10.3, TokenMeta(0, 5)), EofToken)).check()
      }

      "comes bad formatted negative number int" in {
        TestCase("-  10", Seq(NumberToken(-10.0, TokenMeta(0, 5)), EofToken)).check()
      }

      "comes bad formatted negative number double" in {
        TestCase("-  10.3", Seq(NumberToken(-10.3, TokenMeta(0, 7)), EofToken)).check()
      }

      "comes bracket" in {
        Seq("(", ")").foreach(x => TestCase(x, Seq(BracketToken(x == "(", meta(0)), EofToken)).check())
      }

      "comes whitespaces" in {
        Seq(" ", "   ", "\t", System.lineSeparator()).foreach(s => TestCase(s, Seq(WhiteSpaceToken(TokenMeta(0, s.length)), EofToken)).check())
      }

      "comes operators" in {
        val cases = Operators.operators
          .map { op =>
            TestCase(op.pattern, Seq(OperatorToken(op, meta(0)), EofToken))
          }

        cases.foreach(_.check())
      }
    }

    "correctly parse multiple tokens" when {
      "comes '1 + 1'" in {
        val `case` = TestCase(
          "1 + 1",
          Seq(
            NumberToken(1, meta(0)),
            WhiteSpaceToken(meta(1)),
            OperatorToken(Operators.Plus, meta(2)),
            WhiteSpaceToken(meta(3)),
            NumberToken(1, meta(4)),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1 - 1'" in {
        val `case` = TestCase(
          "1 - 1",
          Seq(
            NumberToken(1, meta(0)),
            WhiteSpaceToken(meta(1)),
            OperatorToken(Operators.Minus, meta(2)),
            WhiteSpaceToken(meta(3)),
            NumberToken(1, meta(4)),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1 + -1'" in {
        val `case` = TestCase(
          "1 + -1",
          Seq(
            NumberToken(1, meta(0)),
            WhiteSpaceToken(meta(1)),
            OperatorToken(Operators.Plus, meta(2)),
            WhiteSpaceToken(meta(3)),
            NumberToken(-1, TokenMeta(4, 2)),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1--1'" in {
        val `case` = TestCase(
          "1--1",
          Seq(
            NumberToken(1, meta(0)),
            OperatorToken(Operators.Minus, meta(1)),
            NumberToken(-1, TokenMeta(2, 2)),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1. 0'" in {
        val `case` = TestCase(
          "1. 0",
          Seq(
            NumberToken(1, TokenMeta(0, 2)),
            WhiteSpaceToken(meta(2)),
            NumberToken(0, meta(3)),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1*\\t( 2+3)'" in {
        val `case` = TestCase(
          "1*\t( 2+3)",
          Seq(
            NumberToken(1, meta(0)),
            OperatorToken(Operators.Multiply, meta(1)),
            WhiteSpaceToken(meta(2)),
            BracketToken(isOpen = true, meta(3)),
            WhiteSpaceToken(meta(4)),
            NumberToken(2, meta(5)),
            OperatorToken(Operators.Plus, meta(6)),
            NumberToken(3, meta(7)),
            BracketToken(isOpen = false, meta(8)),
            EofToken
          )
        )
        `case`.check()
      }
    }

    "correctly fail" when {
      "comes unknown chars" in {
        expectedFailOn("mama")
      }

      "comes '2 ^ 2'" in {
        expectedFailOn("2 ^ 2")
      }

      "comes '0xff'" in {
        expectedFailOn("0xff")
      }
    }
  }

}
