package ru.ifkbhit.ppo6.visit

import org.scalatest.{Matchers, WordSpec}
import ru.ifkbhit.ppo6.model.Operators
import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken, Token, TokenMeta}

class RpnCalculateVisitorSpec extends WordSpec with Matchers {
  def exec(tokens: Seq[Token]): Double = {
    val visitor = new RpnCalculateVisitor
    tokens.foreach(_.accept(visitor))

    visitor.produce
  }

  private implicit class RawTokenOps(raw: String) {
    def toToken: Token =
      raw.toDoubleOption
        .map(NumberToken(_, TokenMeta(0, 0)))
        .orElse(Operators.parseSafe(raw).map(OperatorToken(_, TokenMeta(0, 0))))
        .getOrElse(fail(s"incorrect token $raw"))
  }


  private def tokens(s: String): Seq[Token] = s.split(" ").map(_.toToken)


  "RpnCalculateVisitor" should {
    "correctly calculate" when {
      "comes simple operation" in {
        exec(tokens("10 2 +")) shouldBe 12
      }

      "comes non simple operation" in {
        exec(tokens("1 2 3 4 / * 2 / +")) shouldBe (1.0 + 2.0 * (3.0 / 4.0) / 2.0)
      }
    }

    "fail" when {
      "comes to few operators" in {
        an[IllegalStateException] should be thrownBy exec(tokens("1 2 3 +"))
      }

      "comes empty tokens" in {
        an[IllegalStateException] should be thrownBy exec(Seq())
      }

      "comes incorrect rpn" in {
        an[IllegalStateException] should be thrownBy exec(tokens("1 + 1"))
      }

      "comes bracket" in {
        an[UnsupportedOperationException] should be thrownBy exec(Seq(BracketToken(isOpen = true, TokenMeta(0, 0))))
      }
    }
  }

}
