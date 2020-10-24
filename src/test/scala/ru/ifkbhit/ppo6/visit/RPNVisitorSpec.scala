package ru.ifkbhit.ppo6.visit

import org.scalatest.{Assertion, Matchers, WordSpec}
import ru.ifkbhit.ppo6.token.TokenizerImpl

class RPNVisitorSpec extends WordSpec with Matchers {

  private def execute(input: String): String = {
    val tokenizer = new TokenizerImpl
    tokenizer.accept(input)


    val rpn = new RPNVisitor
    tokenizer.tokens.foreach(_.accept(rpn))

    val toStringVisitor = new ToStringVisitor(spacesBetween = true)
    rpn.produce.foreach(_.accept(toStringVisitor))

    toStringVisitor.produce
  }

  private case class TestCase(input: String, expected: String) {
    def check(): Assertion = execute(input) shouldBe expected
  }

  "RPNVisitor" should {
    "correctly produce rpn" when {
      "comes number" in {
        TestCase("10.", "10").check()
      }

      "comes simple binary op" in {
        TestCase("1+1", "1 1 +").check()
      }

      "comes simple expr with 2 same priority operators" in {
        TestCase("1+2-3", "1 2 + 3 -").check()
      }

      "comes simple expr with 2 distinct priority operators" in {
        TestCase("1+2*3", "1 2 3 * +").check()
      }

      "comes simple expr in brackets" in {
        val cases = Seq(
          TestCase("(1 + 2)", "1 2 +"),
          TestCase("(1) + 2", "1 2 +"),
          TestCase("(1) + (2)", "1 2 +"),
          TestCase("1 + (2)", "1 2 +")
        )

        cases.foreach(_.check())
      }

      "comes full expresion" in {
        TestCase(" 1 + 2*(3/4) / 2", "1 2 3 4 / * 2 / +").check()
      }
    }

    "fail" when {
      "comes simple non closed bracket" in {
        an[IllegalStateException] should be thrownBy execute("(1+2")
      }

      "comes not opened close bracket" in {
        an[IllegalStateException] should be thrownBy execute("1+2)")
      }

      "comes expr" in {
        an[IllegalStateException] should be thrownBy execute("((((1+2) * 3) * 4")
      }

      "comes expr#2" in {
        an[IllegalStateException] should be thrownBy execute("((((1+2) * 3) * 4)*5)*6)")
      }
    }
  }
}
