import scala.util.matching._

// Step 1

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

assert(tokens("if(x<0) 0 else root(x);", List("if|def|val".r, """\p{L}(\p{L}|\p{N}|_)*""".r,
  """[+-]?\p{N}+""".r, "[+*/%<=>-]".r, "[(){};]".r, """\p{Z}+""".r), List("""\p{Z}+""".r, """//.*""".r)) == (List("if", "(", "x", "<", "0", ")", "0", "else", "root", "(", "x", ")", ";"), -1))
assert(tokens("if(x<0)& 0 else root(x);", List("if|def|val".r, """\p{L}(\p{L}|\p{N}|_)*""".r,
  """[+-]?\p{N}+""".r, "[+*/%<=>-]".r, "[(){};]".r, """[:.]""".r, "\".*\"".r), List("""\p{Z}+""".r, """//.*""".r)) == (List("if", "(", "x", "<", "0", ")"), 7))

// Step 2

// No recursion!

val characters = (s: String) => s.toList.map("" + _)
characters("72251")

val letters = Map(
  "2" -> "ABC", "3" -> "DEF", "4" -> "GHI", "5" -> "JKL",
  "6" -> "MNO", "7" -> "PRS", "8" -> "TUV", "9" -> "WXY").map(e => (e._1, characters(e._2)))
letters("3").flatMap(y => letters("2").map(x => x + y))

val cats = (s: List[String], t: List[String]) => s.flatMap(y => t.map(x => y + x))
cats(letters("2"), letters("3")).toSet
assert(cats(letters("2"), letters("3")).toSet
  == Set("AD", "BD", "CD", "AE", "BE", "CE", "AF", "BF", "CF"))

val catsSpaces = (s: List[String], t: List[String]) => s.flatMap(y => t.map(x => y + " " + x))
catsSpaces(letters("2"), letters("3")).toSet


val words = io.Source.fromURL("http://horstmann.com/sjsu/spring2018/cs152/words").
  getLines.filter(w => Character.isLowerCase(w(0)) && w.length > 1).
  map(_.toUpperCase).toSet + "SCALA"

val wordsForDigits = (digits: String) => characters(digits).map(x => letters(x)).
  reduceLeft(cats(_,_)).filter(words)
assert(wordsForDigits("72252").contains("SCALA"))
wordsForDigits("72252") // List(SCALA
wordsForDigits("72253") // List(SABLE, SCALD, SCALE)

val wordsForDigitsSequence = (seq: List[String]) => seq.map(e => wordsForDigits(e)).reduceLeft(catsSpaces)
wordsForDigitsSequence(List("72252", "47", "386")) // List(SCALA GS DUN, SCALA GS DUO, SCALA GS FUN, SCALA IS DUN, SCALA IS DUO, SCALA IS FUN)

// Step k - not sure

val grow1 = (c: String, substringLists: List[List[String]]) => substringLists.map(x => c :: x)
grow1("1", List(List("234"),
  List("23", "4"),
  List("2", "34"),
  List("2", "3", "4")))

// Step I - correct
val grow2 = (c: String, substringLists: List[List[String]]) => substringLists.map(x => c + x.head :: x.tail)
grow2("1", List(List("234"),
  List("23", "4"),
  List("2", "34"),
  List("2", "3", "4")))

// Step M

val grow = (c: String, a: List[List[String]]) => grow1(c,a) ++ grow2(c,a)
grow("1", List(List("234"),
  List("23", "4"),
  List("2", "34"),
  List("2", "3", "4")))

assert(grow("1", List(List("234"),
  List("23", "4"),
  List("2", "34"),
  List("2", "3", "4"))).toSet == List(List(1, 234), List(1, 23, 4), List(1, 2, 34), List(1, 2, 3, 4), List(1234), List(123, 4), List(12, 34), List(12, 3, 4)).map(y => y.map(x => x.toString)).toSet)

// Step N
grow("1", grow("2", grow("3", List(List("4"))))) // List(List(1, List(2, List(3, List(4)))), List(1, List(2, List(34))), List(1, List(23, 2List(4))), List(1, List(234)), List(12, 1List(3, List(4))), List(12, 1List(34)), List(123, 12List(4)), List(1234))
                                                  // List(List(1, List(2, List(3, List(4)))), List(1, List(2, List(34))

// Step Q
val substrings = (s: String) => characters(s.substring(0, s.length - 1)).foldRight(List(List(s.substring(s.length-1))))(grow(_,_))
substrings("2728")

// Step T
val phoneMnemonics = (digits: String) => substrings(digits).flatMap(wordsForDigitsSequence)
val result = phoneMnemonics("7225247386")
val length = result.length

assert(phoneMnemonics("7225247386").length == 60)
assert(substrings("2728") == List(List(2, 7, 2, 8), List(2, 7, 28), List(2, 72, 8), List(2, 728), List(27, 2, 8), List(27, 28), List(272, 8), List(2728)).map(x => x.map(y => y.toString)))


