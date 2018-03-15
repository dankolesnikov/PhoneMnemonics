import scala.util.matching._

object hw4 {

  // Problem 1

  def tokens(input : String, patterns : List[Regex], ignored: List[Regex]) : (List[String], Int) = {

    def firstMatch(input : String, patterns : List[Regex]): String = {
      if(patterns.isEmpty) null
      else if(patterns.head.findPrefixOf(input).isDefined) patterns.head.findPrefixOf(input).get
      else firstMatch(input, patterns.tail)
    }

    def helper(input: String, patterns: List[Regex], ignored: List[Regex], result: List[String], position: Int): (List[String], Int) = {
      if (input.isEmpty) (result.reverse, -1) // base case
      else {
        val first = firstMatch(input, patterns)
        val second = firstMatch(input, ignored)
        if (first == null) (result.reverse, position - input.length) // Return the current result when mismatch occurs
        else if (first == second) helper(input.substring(first.length), patterns, ignored, result, position) // Recursive case: don't include the ignore
        else helper(input.substring(first.length), patterns, ignored, first :: result, position)
      }
    }
    helper(input, patterns, ignored, List(),input.length)
  }

  // Problem 2

  val characters = (s: String) => s.toList.map("" + _)

  val letters = Map(
    "2" -> "ABC", "3" -> "DEF", "4" -> "GHI", "5" -> "JKL",
    "6" -> "MNO", "7" -> "PRS", "8" -> "TUV", "9" -> "WXY").map(e => (e._1, characters(e._2)))

  val cats = (s: List[String], t: List[String]) => s.flatMap(y => t.map(x => y + x))

  val catsSpaces = (s: List[String], t: List[String]) => s.flatMap(y => t.map(x => y + " " + x))

  val words = io.Source.fromURL("http://horstmann.com/sjsu/spring2018/cs152/words").
    getLines.filter(w => Character.isLowerCase(w(0)) && w.length > 1).
    map(_.toUpperCase).toSet + "SCALA"

  val wordsForDigits = (digits: String) => characters(digits).map(x => letters(x)).
    reduceLeft(cats(_,_)).filter(words)

  val wordsForDigitsSequence = (seq: List[String]) => seq.map(e => wordsForDigits(e)).
    reduceLeft(catsSpaces)

  val grow1 = (c: String, substringLists: List[List[String]]) => substringLists.map(x => c :: x)

  val grow2 = (c: String, substringLists: List[List[String]]) => substringLists.map(x => c + x.head :: x.tail)

  val grow = (c: String, a: List[List[String]]) => grow1(c,a) ++ grow2(c,a)

  val substrings = (s: String) => characters(s.substring(0, s.length - 1)).foldRight(List(List(s.substring(s.length-1))))(grow(_,_))

  val phoneMnemonics = (digits: String) => substrings(digits).flatMap(wordsForDigitsSequence)

}