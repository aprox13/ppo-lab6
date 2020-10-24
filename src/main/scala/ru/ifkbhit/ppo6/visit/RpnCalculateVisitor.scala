package ru.ifkbhit.ppo6.visit

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken}

import scala.collection.mutable

class RpnCalculateVisitor extends Visitor[Double] {

  private val stack: mutable.Stack[Double] = new mutable.Stack

  override def visitNumber(numberToken: NumberToken): Unit = {
    stack.push(numberToken.value)
  }

  override def visitOperator(operatorToken: OperatorToken): Unit = {
    if (stack.size < 2) {
      throw new IllegalStateException(s"Unexpected operator ${operatorToken.operator}, expected ${if (stack.isEmpty) 2 else 1} number(s)")
    }

    val a = stack.pop()
    val b = stack.pop()
    stack.push(operatorToken.operator.applyTo(a, b))
  }

  override def visitBracket(bracketToken: BracketToken): Unit =
    throw new UnsupportedOperationException("RPN doesn't support brackets")

  override def produce: Double = {
    if (stack.isEmpty) {
      throw new IllegalStateException("Input sequence was empty")
    }

    if (stack.size != 1) {
      throw new IllegalStateException("Wrong input sequence. Found non calculated data")
    }

    stack.pop()
  }
}
