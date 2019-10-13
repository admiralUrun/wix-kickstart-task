import scala.annotation.tailrec

object Messages {
  type NumberOfMessages = Int
  type WordLength = Int

  def functionSolution(s: String, k: Int): NumberOfMessages = {

    def countLengthOfWords(s: String): List[WordLength] = s.split(" ").toList.map(s => s.length)

    @tailrec
    def countMessages(wordsLength: List[WordLength], charsLeft: Int, messages: NumberOfMessages): NumberOfMessages = wordsLength match {
      case List() => -1
      case length :: lengths =>
        val wordWithSpace = 1 + length

        def beginningOfMessage(): Boolean = charsLeft == k

        def shouldSplitMessage(): Boolean = lengths match {
          case List() => false
          case n :: _ =>
            if (beginningOfMessage()) charsLeft == length || charsLeft - length < 1 + n
            else charsLeft == wordWithSpace || charsLeft - wordWithSpace < 1 + n
        }

        if (length > k) -1
        else if (lengths.isEmpty) messages
        else {
          if (shouldSplitMessage()) countMessages(lengths, k, messages + 1)
          else if (beginningOfMessage()) countMessages(lengths, charsLeft - length, messages)
          else countMessages(lengths, charsLeft - wordWithSpace, messages)
        }
    }

    countMessages(countLengthOfWords(s), k, 1)
  }
}
