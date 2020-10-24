package ru.ifkbhit.ppo6.visit

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import org.scalatest.{Matchers, WordSpec}
import ru.ifkbhit.ppo6.token.TokenizerImpl

class PrintStreamPrinterVisitorSpec extends WordSpec with Matchers {

  "PrintStreamPrinterVisitor" should {
    "correctly visit" in {
      val input = "   10 +\t\n10.2 - (1*1)"
      val expected = "10+10.2-(1*1)"

      val tokenizer = new TokenizerImpl
      tokenizer.accept(input)

      val os = new ByteArrayOutputStream
      val ps = new PrintStream(os)

      val visitor = new PrintStreamPrinterVisitor(ps)
      tokenizer.tokens.foreach(_.accept(visitor))

      os.toString(StandardCharsets.UTF_8.name()) shouldBe expected
    }
  }
}
