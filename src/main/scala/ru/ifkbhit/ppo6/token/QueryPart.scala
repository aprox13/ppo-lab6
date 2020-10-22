package ru.ifkbhit.ppo6.token

import ru.ifkbhit.ppo6.token.Tokens.Token

case class QueryPart(token: Token, value: String, index: Int) {
  def size: Int = value.length
}
