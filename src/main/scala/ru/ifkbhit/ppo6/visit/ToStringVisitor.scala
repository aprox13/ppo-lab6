package ru.ifkbhit.ppo6.visit

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken}

class ToStringVisitor(spacesBetween: Boolean = false) extends Visitor[String] {

  private val os = new ByteArrayOutputStream
  private val ps = new PrintStream(os)
  private val printVisitor = new PrintStreamPrinterVisitor(ps, spacesBetween)

  override def visitNumber(numberToken: NumberToken): Unit = printVisitor.visitNumber(numberToken)

  override def visitOperator(operatorToken: OperatorToken): Unit = printVisitor.visitOperator(operatorToken)

  override def visitBracket(bracketToken: BracketToken): Unit = printVisitor.visitBracket(bracketToken)

  override def produce: String = {
    val res = os.toString(StandardCharsets.UTF_8.name())
    if (spacesBetween) {
      res.dropRight(1)
    } else {
      res
    }
  }
}
