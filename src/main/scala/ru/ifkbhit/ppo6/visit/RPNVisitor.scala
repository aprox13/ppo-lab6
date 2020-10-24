package ru.ifkbhit.ppo6.visit

import ru.ifkbhit.ppo6.token.{BracketToken, NumberToken, OperatorToken, Token}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class RPNVisitor extends Visitor[Seq[Token]] {

  private val result: ArrayBuffer[Token] = ArrayBuffer()
  private val stack: mutable.Stack[Token] = mutable.Stack()

  override def visitNumber(numberToken: NumberToken): Unit =
    result += numberToken

  override def visitOperator(operatorToken: OperatorToken): Unit = {
    stackToResultWhile(token => getTokenWeight(token) >= getTokenWeight(operatorToken))
    stack.push(operatorToken)
  }

  private def stackToResultWhile(f: Token => Boolean): Unit =
    result ++= stack.popWhile(f)

  private def getTokenWeight(token: Token): Int =
    token match {
      case BracketToken(true, _) => // open bracket
        -1
      case OperatorToken(operator, _) =>
        operator.weight
      case _ =>
        throw new IllegalStateException(s"Unsupported weight get for token $token")
    }

  override def visitBracket(bracketToken: BracketToken): Unit = {
    if (bracketToken.isOpenBracket) {
      stack.push(bracketToken)
    } else {
      stackToResultWhile(!_.isOpenBracket)
      if (!stack.headOption.exists(_.isOpenBracket)) {
        throw new IllegalStateException(s"Unexpected close bracket at index ${bracketToken.tokenMeta.startIndex}")
      }
      stack.pop() // remove open bracket
    }
  }

  override def produce: Seq[Token] = {
    stackToResultWhile(!_.isOpenBracket) // move all to result
    if (stack.nonEmpty) {
      throw new IllegalStateException(s"Unexpected open bracket, index: ${stack.top.tokenMeta.startIndex}")
    }
    result.toSeq
  }

}
