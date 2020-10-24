package ru.ifkbhit.ppo6.token

import org.scalatest.{Assertion, Matchers, WordSpec}
import ru.ifkbhit.ppo6.model.Operators

class TokenizerSpec extends WordSpec with Matchers {

  private def expectedFailOn(source: String) = {
    val tokenizer = new TokenizerImpl

    an[IllegalArgumentException] should be thrownBy tokenizer.accept(source)
  }

  private case class TestCase(source: String, expected: Seq[Token]) {
    def check(): Assertion = {
      val tokenizer = new TokenizerImpl
      tokenizer.accept(source)

      tokenizer.tokens should contain theSameElementsInOrderAs expected
    }
  }

  "Tokenizer" should {
    "correctly parse single tokens" when {
      "comes nothing" in {
        TestCase("", Seq(EofToken)).check()
      }

      "comes positive number int" in {
        TestCase("10", Seq(NumberToken(10.0, 0), EofToken)).check()
      }

      "comes negative number int" in {
        TestCase("-10", Seq(NumberToken(-10.0, 0), EofToken)).check()
      }

      "comes positive number double" in {
        TestCase("10.3", Seq(NumberToken(10.3, 0), EofToken)).check()
      }

      "comes negative number double" in {
        TestCase("-10.3", Seq(NumberToken(-10.3, 0), EofToken)).check()
      }

      "comes bad formatted negative number int" in {
        TestCase("-  10", Seq(NumberToken(-10.0, 0), EofToken)).check()
      }

      "comes bad formatted negative number double" in {
        TestCase("-  10.3", Seq(NumberToken(-10.3, 0), EofToken)).check()
      }

      "comes bracket" in {
        Seq("(", ")").foreach(x => TestCase(x, Seq(BracketToken(x == "(", 0), EofToken)).check())
      }

      "comes whitespaces" in {
        Seq(" ", "   ", "\t", System.lineSeparator()).foreach(TestCase(_, Seq(WhiteSpaceToken(0), EofToken)).check())
      }

      "comes operators" in {
        val cases = Operators.operators
          .map { op =>
            TestCase(op.pattern, Seq(OperatorToken(op, 0), EofToken))
          }

        cases.foreach(_.check())
      }
    }

    "correctly parse multiple tokens" when {
      "comes '1 + 1'" in {
        val `case` = TestCase(
          "1 + 1",
          Seq(
            NumberToken(1, 0),
            WhiteSpaceToken(1),
            OperatorToken(Operators.Plus, 2),
            WhiteSpaceToken(3),
            NumberToken(1, 4),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1 + -1'" in {
        val `case` = TestCase(
          "1 + -1",
          Seq(
            NumberToken(1, 0),
            WhiteSpaceToken(1),
            OperatorToken(Operators.Plus, 2),
            WhiteSpaceToken(3),
            NumberToken(-1, 4),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1--1'" in {
        val `case` = TestCase(
          "1--1",
          Seq(
            NumberToken(1, 0),
            OperatorToken(Operators.Minus, 1),
            NumberToken(-1, 2),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1. 0'" in {
        val `case` = TestCase(
          "1. 0",
          Seq(
            NumberToken(1, 0),
            WhiteSpaceToken(2),
            NumberToken(0, 3),
            EofToken
          )
        )
        `case`.check()
      }

      "comes '1*\\t( 2+3)'" in {
        val `case` = TestCase(
          "1*\t( 2+3)",
          Seq(
            NumberToken(1, 0),
            OperatorToken(Operators.Multiply, 1),
            WhiteSpaceToken(2),
            BracketToken(isOpen = true, 3),
            WhiteSpaceToken(4),
            NumberToken(2, 5),
            OperatorToken(Operators.Plus, 6),
            NumberToken(3, 7),
            BracketToken(isOpen = false, 8),
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
