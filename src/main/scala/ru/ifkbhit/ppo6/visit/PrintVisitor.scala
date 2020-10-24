package ru.ifkbhit.ppo6.visit

import java.io.PrintStream

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken}

class PrintStreamPrinterVisitor(ps: PrintStream, spacesBetween: Boolean) extends Visitor[Unit] {

  private val Delimiter: String = if (spacesBetween) " " else ""

  override def visitNumber(numberToken: NumberToken): Unit = ps.print(numberToString(numberToken) + Delimiter)

  private def numberToString(numberToken: NumberToken): String = {
    import numberToken._

    if (value != value.toInt.toDouble) {
      value.toString
    } else {
      value.toInt.toString
    }
  }

  override def visitOperator(operatorToken: OperatorToken): Unit = ps.print(operatorToken.operator.pattern + Delimiter)

  override def visitBracket(bracketToken: BracketToken): Unit = ps.print(bracketToString(bracketToken) + Delimiter)

  private def bracketToString(bracketToken: BracketToken): String =
    if (bracketToken.isOpen) "(" else ")"

  override def produce: Unit = {}
}


case object PrintVisitor extends PrintStreamPrinterVisitor(System.out, spacesBetween = true)
