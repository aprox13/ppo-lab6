package ru.ifkbhit.ppo6.token


class Tokenizer(source: String) {

  def tokens: Iterator[QueryPart] = {
    val stub: QueryPart = null

    Iterator.iterate((0, stub)) {
      case (idx, _) =>
        val s = source.drop(idx)

        val suitable = Tokens.allTokens
          .flatMap { token =>
            token.matches(s).map { value =>
              QueryPart(token, value, idx)
            }
          }

        require(
          suitable.nonEmpty || s.isEmpty,
          s"Unexpected characters at position ${idx + 1}: '${s.charAt(0)}'"
        )
        require(suitable.size == 1, buildIntersectMessage(suitable, idx))

        idx + suitable.head.size -> suitable.head
    }
      .drop(1) // drop stub
      .map(_._2)
      .takeWhile(_.token != Tokens.EOF)
  }

  private def buildIntersectMessage(parts: Seq[QueryPart], idx: Int): String = {
    val partsStringifies: String =
      parts.map {
        case QueryPart(token, value, _) =>
          s"value '$value' from token $token"
      }.mkString(", ")

    s"Got ${parts.size} intersections at position ${idx + 1}: $partsStringifies"
  }

}
